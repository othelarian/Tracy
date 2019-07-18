module Project exposing(Model, Msg(..), createNewProject, init, update, view)

import Api exposing (..)
import Graphics exposing (..)
import JsonData exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html, button, div, hr, input, label, option, p, select, span, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Html.Extra as Html
import Http
import Json.Decode as JD
import Json.Decode exposing (Decoder, field, int, list, string)
import Json.Encode as JE
import Markdown

import Debug

-- MODEL

type alias ProjectError =
    { phase : ProjectPhase
    , info : String
    }

type alias Step = Int

type ProjectPhase
    = Loading ProjectInfo
    | Failing ProjectError
    | Viewing
    | Editing
    | Saving Step

type alias Model =
    { apiCredentials : ApiCredentials
    , projectInfo : ProjectInfo
    , homeId : FileId
    , tmpName : String
    , tmpDesc : String
    , tmpWaitSave : Bool
    , phase : ProjectPhase
    , base : ProjectBase
    , testValue : String
    , testShow : Bool
    }

init : ApiCredentials -> ProjectInfo -> FileId -> Bool -> Model
init apiCredentials projectInfo homeId testShow =
    Model
        apiCredentials
        projectInfo
        homeId
        projectInfo.name
        ""
        False
        (Loading projectInfo)
        (ProjectBase "" 0 Dict.empty [])
        ""
        testShow

-- HANDLERS

createNewProject : String -> ProjectBase
createNewProject desc =
    ProjectBase desc 0 Dict.empty []

findParentIds : Dict String ProjectTask -> String -> List String -> List String
findParentIds tasks parentId acc =
    case Dict.get parentId tasks of
        Just foundTask -> findParentIds tasks foundTask.parentId (parentId::acc)
        Nothing -> acc

findSubTaskIds : Dict String ProjectTask -> String -> List String -> List String -> List String
findSubTaskIds tasks subTaskId future acc =
    let
        (newFuture, newAcc) =
            case Dict.get subTaskId tasks of
                Just subTask -> (List.append future subTask.subTasks, subTaskId::acc)
                Nothing -> (future, acc)
    in
    case newFuture of
        nextId::tail -> findSubTaskIds tasks nextId tail newAcc
        _ -> newAcc

handleError : Model -> ProjectPhase -> Http.Error -> (Model, Cmd Msg)
handleError model phase error =
    ({model | phase = Failing (ProjectError phase (httpErrorToString error))}, Cmd.none)

handleUpdateHome : Model -> List ProjectInfo -> (Model, Cmd Msg)
handleUpdateHome model projects =
    let
        letChangeProject projectInfo =
            if projectInfo.fileId == model.projectInfo.fileId then
                {projectInfo | name = model.tmpName, indicators = model.projectInfo.indicators}
            else projectInfo
        modProjects = List.map letChangeProject projects
        homeValue = encodeHome modProjects
    in
    ( {model | phase = Saving 2}
    , apiUpdateFile (FSUpdate model.homeId homeValue) ValidStep2Edit model.apiCredentials)

handleUpdateProject : Model -> (Model, Cmd Msg)
handleUpdateProject model =
    let
        oldProjectInfo = model.projectInfo
        newProjectInfo = {oldProjectInfo | name = model.tmpName}
        oldBase = model.base
        newBase = {oldBase | desc = model.tmpDesc}
        projectValue = encodeProject newBase
    in
    ( {model | phase = Saving 3, base = newBase, projectInfo = newProjectInfo}
    , apiUpdateFile (FSUpdate model.projectInfo.fileId projectValue) ValidStep3Edit model.apiCredentials)

handleUpdateTask : String -> Model -> Msg -> (Model, Cmd Msg)
handleUpdateTask taskId model msg =
    case Dict.get taskId model.base.tasks of
        Just task ->
            let
                updateTask maybe = case maybe of
                    Just oldTask ->
                        case msg of
                            OpenTask _ -> Just {oldTask | opened = not task.opened}
                            CancelTask _ -> Just {oldTask | mode = ModeView}
                            ValidTask _ ->
                                Just {oldTask | mode = ModeView, opened = True, title = task.tmpTitle, desc = task.tmpDesc}
                            EditTask _ -> Just {oldTask | mode = ModeEdit, tmpTitle = task.title, tmpDesc = task.desc}
                            UpdateTaskTitle _ newTitle -> Just {oldTask | tmpTitle = newTitle}
                            UpdateTaskDesc _ newDesc -> Just {oldTask | tmpDesc = newDesc}
                            RemoveTask _ -> Just {oldTask | mode = ModeRemove}
                            _ -> Just oldTask
                    Nothing -> Nothing
                newTasks = Dict.update taskId updateTask model.base.tasks
                oldBase = model.base
            in
            ({model | base = {oldBase | tasks = newTasks}, tmpWaitSave = True}, Cmd.none)
        Nothing -> (model, Cmd.none)

-- UPDATE

type Msg
    = LoadProject ProjectInfo (Result Http.Error ProjectBase)
    | Retry ProjectError
    | AskEdit
    | CancelEdit
    | UpdateEditName String
    | UpdateEditDesc String
    | SaveEdit
    | ValidStep1Edit (Result Http.Error (List ProjectInfo))
    | ValidStep2Edit (Result Http.Error String)
    | ValidStep3Edit (Result Http.Error String)
    | AddTask String
    | OpenTask String
    | CancelTask String
    | ValidTask String
    | EditTask String
    | UpdateTaskTitle String String
    | UpdateTaskDesc String String
    | UpdateTaskStatus String String
    | MoveTask
    | RemoveTask String
    | DeleteTask String
    | GoToHome ApiCredentials

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadProject infoFile result ->
            case result of
                Ok projectBase ->
                    ({model | phase = Viewing , base = projectBase , tmpDesc = projectBase.desc}, Cmd.none)
                Err error -> handleError model model.phase error
        Retry error ->
            case error.phase of
                Loading projectInfo ->
                    ( {model | phase = Loading projectInfo}
                    , apiReadFile (FSRead projectInfo.fileId) (LoadProject projectInfo) decodeProject model.apiCredentials)
                Saving step ->
                    if step == 1 then
                        ( {model | phase = Saving 1}
                        , apiReadFile (FSRead model.homeId) ValidStep1Edit decodeHome model.apiCredentials)
                    else
                        let
                            oldBase = model.base
                            newBase = {oldBase | desc = model.tmpDesc}
                            projectValue = encodeProject newBase
                        in
                        ( {model | phase = Saving 3}
                        , apiUpdateFile (FSUpdate model.projectInfo.fileId projectValue) ValidStep3Edit model.apiCredentials)
                _ -> (model, Cmd.none)
        AskEdit -> ({model | phase = Editing}, Cmd.none)
        CancelEdit ->
            ({model | phase = Viewing, tmpName = model.projectInfo.name, tmpDesc = model.base.desc}, Cmd.none)
        UpdateEditName value -> ({model | tmpName = value}, Cmd.none)
        UpdateEditDesc value -> ({model | tmpDesc = value}, Cmd.none)
        SaveEdit ->
            ( {model | phase = Saving 1}
            , apiReadFile (FSRead model.homeId) ValidStep1Edit decodeHome model.apiCredentials)
        ValidStep1Edit result ->
            case result of
                Ok projects -> handleUpdateHome model projects
                Err error -> handleError model model.phase error
        ValidStep2Edit result ->
            case result of
                Ok _ -> handleUpdateProject model
                Err error -> handleError model (Saving 1) error
        ValidStep3Edit result ->
            case result of
                Ok _ -> ({model | phase = Viewing, tmpWaitSave = False} , Cmd.none)
                Err error -> handleError model (Saving 3) error
        AddTask parentId ->
            let
                -- create new task
                oldBase = model.base
                newTask = JsonData.generateTask True ("Nouvelle tâche "++(String.fromInt oldBase.nextId)) parentId
                -- update parent tasks
                moreIndicators = ProjectIndicators 1 0 0
                parentTaskIds = findParentIds model.base.tasks parentId []
                --
                updateTask foldParentId (accTasks, accIndicators) =
                    --
                    -- TODO : mise à jour des indicateurs suivant si des tâches parentes basculent leur statut
                    -- TODO : mise à jour des statuts des tâches parentes
                    --
                    (accTasks, accIndicators)
                    --
                --
                (updatedTasks, finalIndicators) = List.foldr updateTask (model.base.tasks, moreIndicators) parentTaskIds
                --
                --
                --
                newBase =
                    {oldBase
                        | nextId = (oldBase.nextId + 1)
                        , tasks = Dict.insert (String.fromInt oldBase.nextId) newTask updatedTasks
                        }
                --
                {-
                oldProjectInfo = model.projectInfo
                newProjectInfo = {oldProjectInfo | values = newValues}
                newModel = {model | projectInfo = newProjectInfo, tmpWaitSave = True}
                -}
            in
            {-
            case parentId of
                "-1" ->
                    let newOrder = Array.push (String.fromInt oldBase.nextId) oldBase.topLevel in
                    ({newModel | base = {newBase | topLevel = newOrder}}, Cmd.none)
                _ ->
                    let
                        oldParent = case Dict.get parentId oldBase.tasks of
                            Just task -> task
                            Nothing -> JsonData.generateTask False "" ""
                        newOrder = Array.push (String.fromInt oldBase.nextId) oldParent.subTasks
                        updateParent maybe = case maybe of
                            Just parent -> Just {parent | subTasks = newOrder}
                            Nothing -> Nothing
                        newTasks = Dict.update parentId updateParent oldBase.tasks
                    in
                    ({newModel | base = {newBase | tasks = newTasks}} , Cmd.none)
            -}
            --
            --
            (model, Cmd.none)
            --
        OpenTask taskId -> handleUpdateTask taskId model msg
        CancelTask taskId -> handleUpdateTask taskId model msg
        ValidTask taskId -> handleUpdateTask taskId model msg
        EditTask taskId -> handleUpdateTask taskId model msg
        UpdateTaskTitle taskId newTitle -> handleUpdateTask taskId model msg
        UpdateTaskDesc taskId newDesc -> handleUpdateTask taskId model msg
        UpdateTaskStatus taskId newStatusString ->
            case Dict.get taskId model.base.tasks of
                Just task ->
                    --
                    --
                    -- TODO : mise à jour du parent si nécessaire à faire
                    --
                    {-
                    --
                    let
                        newStatus = case newStatusString of
                            "planned" -> Planned
                            "wip" -> Wip
                            "closed" -> Closed
                            _ -> task.status
                        oldProjectInfo = model.projectInfo
                        newProjectInfo =
                            let
                                valueId tmpStatus = case tmpStatus of
                                    Planned -> 0
                                    Wip -> 1
                                    Closed -> 2
                                oldStartValue = case Array.get (valueId task.status) oldProjectInfo.values of
                                    Just value -> value
                                    Nothing -> 1
                                oldEndValue = case Array.get (valueId newStatus) oldProjectInfo.values of
                                    Just value -> value
                                    Nothing -> 0
                                newValues =
                                    Array.set (valueId task.status) (oldStartValue-1) oldProjectInfo.values
                                        |> Array.set (valueId newStatus) (oldEndValue+1)
                            in
                            {oldProjectInfo | values = newValues}
                        updateTask maybe = case maybe of
                            Just oldTask -> Just {oldTask | status = newStatus}
                            Nothing -> Nothing
                        newTasks = Dict.update taskId updateTask model.base.tasks
                        oldBase = model.base
                    in
                    ({model | projectInfo = newProjectInfo, base = {oldBase | tasks = newTasks}, tmpWaitSave = True}, Cmd.none)
                    -}
                    --
                    --
                    --
                    (model, Cmd.none)
                    --
                Nothing -> (model, Cmd.none)
        MoveTask ->
            --
            --
            (model, Cmd.none)
            --
        RemoveTask taskId -> handleUpdateTask taskId model msg
        DeleteTask taskId ->
            case Dict.get taskId model.base.tasks of
                Just task ->
                    let
                        -- get subTasks and remove them
                        allSubTaskIds = case task.subTasks of
                            head::tail -> findSubTaskIds model.base.tasks head tail []
                            _ -> []
                        doPurge purgedKey tmpDict = Dict.remove purgedKey tmpDict
                        purgedTasks = List.foldl doPurge model.base.tasks allSubTaskIds
                        -- update parent tasks with new calculated values
                        parentTaskIds = findParentIds purgedTasks task.parentId []
                        updateTask parentId (tmpTasks, lessIndicators) =
                            case Dict.get parentId tmpTasks of
                                Just parentTask ->
                                    let
                                        oldParentIndicators = parentTask.indicators
                                        newParentIndicators =
                                            {oldParentIndicators
                                                | wait = oldParentIndicators.wait + lessIndicators.wait
                                                , wip = oldParentIndicators.wip + lessIndicators.wip
                                                , done = oldParentIndicators.done + lessIndicators.done
                                                }
                                        (newParentStatus, updateLessIndicators) =
                                            case parentTask.status of
                                                Planned -> (Planned, lessIndicators)
                                                Wip ->
                                                    if newParentIndicators.wait == 0 && newParentIndicators.wip == 0 then
                                                        ( Closed
                                                        , {lessIndicators
                                                            | wip = lessIndicators.wip - 1
                                                            , done = lessIndicators.done + 1
                                                            }
                                                        )
                                                    else if newParentIndicators.wip == 0 && newParentIndicators.done == 0 then
                                                        ( Planned
                                                        , {lessIndicators
                                                            | wait = lessIndicators.wait + 1
                                                            , wip = lessIndicators.wip - 1
                                                            }
                                                        )
                                                    else (Wip, lessIndicators)
                                                Closed -> (Closed, lessIndicators)
                                        doUpdate maybeParentTask =
                                            case maybeParentTask of
                                                Just tmpParentTask ->
                                                    Just {tmpParentTask | indicators = newParentIndicators, status = newParentStatus}
                                                Nothing -> Nothing
                                    in
                                    (Dict.update parentId doUpdate tmpTasks, updateLessIndicators)
                                Nothing -> (tmpTasks, lessIndicators)
                        removeIndicators =
                            let
                                tmpIndicators =
                                    ProjectIndicators
                                        -task.indicators.wait
                                        -task.indicators.wip
                                        -task.indicators.done
                            in
                            case task.status of
                                Planned -> {tmpIndicators | wait = tmpIndicators.wait - 1}
                                Wip -> {tmpIndicators | wait = tmpIndicators.wip - 1}
                                Closed -> {tmpIndicators | wait = tmpIndicators.done - 1}
                        (updatedTasks, newIndicators) = List.foldr updateTask (purgedTasks, removeIndicators) parentTaskIds
                        -- update immediate parent task
                        updateImmediateParent maybeParentTask =
                            let newParentSubTasks tmpTask = List.filter (\n -> n /= task.parentId) tmpTask.subTasks in
                            case maybeParentTask of
                                Just parentTask -> Just {parentTask | subTasks = newParentSubTasks parentTask}
                                Nothing -> Nothing
                        finalTasks = Dict.update task.parentId updateImmediateParent updatedTasks
                        -- update project values
                        oldProjectInfo = model.projectInfo
                        newProjectInfo = {oldProjectInfo | indicators = newIndicators}
                        -- final steps
                        newTasks = Dict.remove taskId finalTasks
                        oldBase = model.base
                    in
                    ({model | projectInfo = newProjectInfo, base = {oldBase | tasks = newTasks}}, Cmd.none)
                Nothing -> (model, Cmd.none)
        GoToHome _ -> (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
    div [class "core"]
        [ div [class "zone_status"]
            (case model.phase of
                Loading infoFile -> [span [] [text ("Projet : "++infoFile.name)]]
                Failing error ->
                    [ span [class "error"] [text "Il y a eu un problème !"]
                    , button [onClick (Retry error), class "button_topsnap"] [text "Réessayer"]
                    ]
                Viewing ->
                    [ span [] [text model.projectInfo.name]
                    , button [onClick (GoToHome model.apiCredentials), class "button_topsnap"] [iconClose]
                    , button [onClick AskEdit, class "button_topsnap"] [iconEdit]
                    ]
                Editing -> [span [] [text model.projectInfo.name]]
                Saving _ -> [span [] [text "Mise à jour du projet"]]
            )
        , (case model.phase of
            Viewing ->
                div [class "project_tracker"] (List.append
                        --
                        --
                        -- TODO : sortir la div project_tracker pour la réutiliser
                        --
                        --
                        (let
                            indicators = model.projectInfo.indicators
                            total = indicators.wait + indicators.wip + indicators.done
                        in
                        if total > 0 then
                            if total == indicators.done then
                                [ span
                                    [class "project_end"]
                                    [text ("Vous avez terminé ce projet ! (soit "++(String.fromInt total)++" tâches)")]
                                ]
                            else
                                [ progressBar indicators
                                , div [class "project_progress"]
                                    [ span [class "round_box wait_color"] [text (String.fromInt indicators.wait)]
                                    , span [class "round_box wip_color"] [text (String.fromInt indicators.wip)]
                                    , span [class "round_box done_color"] [text (String.fromInt indicators.done)]
                                    , span [class "round_box total_color"] [text (String.fromInt total)]
                                    ]
                                ]
                        else [Html.nothing])
                    [ button [class "button_bottomsnap", onClick (AddTask "-1")] [iconAdd]
                    ,
                        if model.tmpWaitSave then button [class "button_bottomsnap", onClick SaveEdit] [iconValid]
                        else Html.nothing
                    , hr [style "clear" "both"] []
                    ])
            _ -> Html.nothing
            )
        , (case model.phase of
            Loading _ -> div [class "waiter"] [text "Chargement du projet en cours, veuillez patienter"]
            Failing error -> div [class "error"] [text error.info]
            Viewing ->
                div []
                    [ (div [class "project_desc"] [
                        (if (String.isEmpty (String.trim model.base.desc)) then text "(Description absente)"
                        else Markdown.toHtml [] model.base.desc)])
                    , hr [] []
                    , div [] (viewTasks model.base.tasks model.base.topLevel)
                    ]
            Editing ->
                div []
                    [ p []
                        [ label [for "project_name"] [text "Nom : "]
                        , input
                            [ id "project_name"
                            , value model.tmpName
                            , onInput UpdateEditName
                            , maxlength 20
                            , size 23
                            ] []
                        ]
                    , p [] [label [for "project_desc"] [text "Description :"]]
                    , textarea
                        [id "project_desc", onInput UpdateEditDesc, rows 7]
                        [text model.tmpDesc]
                    , p [class "centered"]
                        [ button [onClick CancelEdit, class "button_round"] [iconClose]
                        , button [onClick SaveEdit, class "button_round"] [iconValid]
                        ]
                    ]
            Saving step ->
                div [class "waiter"] [
                    text (case step of
                        1 -> "Récupération des informations ..."
                        2 -> "Sauvegarde dans la racine ..."
                        3 -> "Sauvegarde du fichier projet ..."
                        _ -> ""
                    )]
        )
        ]

viewTasks : Dict String ProjectTask -> List String -> List (Html Msg)
viewTasks tasks taskIds =
    List.map (viewTask tasks) taskIds

viewTask : Dict String ProjectTask -> String -> Html Msg
viewTask tasks taskId =
    case Dict.get taskId tasks of
        Just task ->
            let
                statusColor = case task.status of
                    Planned -> "wait_color"
                    Wip -> "wip_color"
                    Closed -> "done_color"
                taskClasses = "task_bar round_box "++statusColor
                statusSelector =
                    select [class statusColor, onInput (UpdateTaskStatus taskId)]
                        [ option [class "wait_color", value "planned", selected (task.status == Planned)] [text "Attente"]
                        , option [class "wip_color", value "wip", selected (task.status == Wip)] [text "En cours"]
                        , option [class "done_color", value "closed", selected (task.status == Closed)] [text "Terminée"]
                        ]
                onCustomClick msg =
                    stopPropagationOn "click" (JD.map (\n -> (n, True)) (JD.succeed msg)) 
                (actions, buttons, content) = case task.mode of
                    ModeEdit ->
                        ( [class taskClasses]
                        ,
                            [ button [onCustomClick (ValidTask taskId), class "button_round"] [iconValid]
                            , button [onCustomClick (CancelTask taskId), class "button_round"] [iconClose]
                            , statusSelector
                            ]
                        , [textarea [onInput (UpdateTaskDesc taskId), rows 7] [text task.tmpDesc]]
                        )
                    ModeView ->
                        let
                            draggable = case task.opened of
                                True -> []
                                False ->
                                    --
                                    -- TODO : rendre draggable, normalement c'est ici
                                    --
                                    []
                                    --
                            --
                        in
                        --
                        ( (class taskClasses)::(onClick (OpenTask taskId))::(draggable)
                        --
                        ,
                            [ button [onCustomClick (RemoveTask taskId), class "button_round"] [iconClose]
                            , button [onCustomClick (EditTask taskId), class "button_round"] [iconEdit]
                            , statusSelector
                            ]
                        , case task.opened of
                            True ->
                                let
                                    subTasksList =
                                        if List.isEmpty task.subTasks then [Html.nothing]
                                        else [div [class "project_subtasks"] (viewTasks tasks task.subTasks)]
                                    --
                                    -- TODO : récupérer depuis les sous tâches combien sont terminées, en attente, etc
                                    --
                                    --
                                    --
                                in
                                List.concat
                                    [ [div [] [Markdown.toHtml [] task.desc]]
                                    --
                                    -- TODO : ajouter de quoi créer des tâches
                                    -- TODO : s'il y a des sous tâches, alors il faut mettre une progress bar !
                                    --
                                    , (
                                        if List.isEmpty task.subTasks then
                                        --
                                        --
                                        [text ""]
                                        else [div [] [text "Liste des sous-tâches :"]]
                                    )
                                    ,
                                        --
                                        --
                                        -- TODO : rendre ce bouton d'ajout actif -> , onClick (AddTask taskId)
                                        --
                                        [ button [class "button_bottomsnap", style "float" "right"] [iconAdd]
                                        --
                                        , hr [style "clear" "both"] []
                                        ]
                                    , (subTasksList)
                                    ]
                            False -> [Html.nothing]
                        )
                    ModeRemove ->
                        ([class taskClasses], [],
                            [ p [class "centered"] [text "Êtes-vous sûr de vouloir supprimer cette tâche ?"]
                            , p [class "centered"]
                                [ button [onClick (CancelTask taskId), class "button_round"] [iconClose]
                                , button [onClick (DeleteTask taskId), class "button_round"] [iconValid]
                                ]
                        ])
            in
            div []
                (div actions
                    ((case task.mode of
                        ModeEdit ->
                            input
                                [ value task.tmpTitle
                                , onInput (UpdateTaskTitle taskId)
                                , maxlength 25
                                , size 28
                                ] []
                        _ -> text task.title
                    )::buttons)::content)
        Nothing -> Html.nothing    
