module Project exposing(Model, Msg(..), createNewProject, subscriptions, init, update, view)

import Api exposing (..)
import Graphics exposing (..)
import JsonData exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import DnDList
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Extra as Html
import Http
import Json.Decode as JD
import Json.Decode exposing (Decoder, field, int, list, string)
import Json.Encode as JE
import Markdown
import Url

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
    { apiToken : ApiToken
    , url : Url.Url
    , projectInfo : ProjectInfo
    , homeId : FileId
    , tmpName : String
    , tmpDesc : String
    , tmpPreview : Bool
    , tmpWaitSave : Bool
    , dnd : DnDList.Model
    , dndTasks : List DnDTask
    , dndShow : Bool
    , phase : ProjectPhase
    , base : ProjectBase
    , testValue : String
    , testShow : Bool
    }

init : ApiToken -> Url.Url -> ProjectInfo -> FileId -> Bool -> Model
init apiToken url projectInfo homeId testShow =
    Model
        apiToken
        url
        projectInfo
        homeId
        projectInfo.name
        ""
        False
        False
        dndSystem.model
        []
        False
        (Loading projectInfo)
        (ProjectBase "" 0 Dict.empty [])
        ""
        testShow

-- DND SYSTEM

type alias DnDTask = TaskId

dndConfig : DnDList.Config DnDTask
dndConfig =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Free
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }

dndSystem : DnDList.System DnDTask Msg
dndSystem = DnDList.create dndConfig DnDMsg

-- HANDLERS

calculateParentUpdate : TaskId -> Dict TaskId ProjectTask -> Dict TaskId ProjectTask
calculateParentUpdate parentId accTasks =
    let
        updateParentIndicators maybeParentTask =
            case maybeParentTask of
                Just parentTask ->
                    let
                        newParentIndicators = calculateSubTasksIndicators parentTask.subTasks accTasks
                        newParentStatus =
                            if newParentIndicators.wip == 0 then
                                if newParentIndicators.done == 0 then Planned
                                else if newParentIndicators.wait == 0 then Closed
                                else Wip
                            else Wip
                    in
                    Just {parentTask | indicators = newParentIndicators, status = newParentStatus}
                Nothing -> Nothing
    in
    Dict.update parentId updateParentIndicators accTasks

calculateSubTasksIndicators : List TaskId -> Dict TaskId ProjectTask -> ProjectIndicators
calculateSubTasksIndicators subTaskIds calcTasks =
    let
        getSubTaskIndicators subTaskId accIndicators =
            case Dict.get subTaskId calcTasks of
                Just subTask ->
                    let tmpIndicators = handleUpdateIndicators accIndicators subTask.indicators in
                    case subTask.status of
                        Planned -> {tmpIndicators | wait = tmpIndicators.wait + 1}
                        Wip -> {tmpIndicators | wip = tmpIndicators.wip + 1}
                        Closed -> {tmpIndicators | done = tmpIndicators.done + 1}
                Nothing -> accIndicators
    in
    List.foldl getSubTaskIndicators (ProjectIndicators 0 0 0) subTaskIds

createNewProject : String -> ProjectBase
createNewProject desc =
    ProjectBase desc 0 Dict.empty []

findParentIds : Dict TaskId ProjectTask -> TaskId -> List TaskId -> List TaskId
findParentIds tasks parentId acc =
    case Dict.get parentId tasks of
        Just foundTask -> findParentIds tasks foundTask.parentId (parentId::acc)
        Nothing -> acc

findSubTaskIds : Dict TaskId ProjectTask -> TaskId -> List TaskId -> List TaskId -> List TaskId
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
    , updateFile (FSUpdate model.homeId homeValue) ValidStep2Edit model.apiToken.accessToken)

handleUpdateIndicators : ProjectIndicators -> ProjectIndicators -> ProjectIndicators
handleUpdateIndicators prevIndicators addedIndicators =
    {prevIndicators
        | wait = prevIndicators.wait + addedIndicators.wait
        , wip = prevIndicators.wip + addedIndicators.wip
        , done = prevIndicators.done + addedIndicators.done
        }

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
    , updateFile (FSUpdate model.projectInfo.fileId projectValue) ValidStep3Edit model.apiToken.accessToken)

handleUpdateTask : String -> Model -> Msg -> (Model, Cmd Msg)
handleUpdateTask taskId model msg =
    case Dict.get taskId model.base.tasks of
        Just task ->
            let
                doINeedSaving =
                    case msg of
                        OpenTask _ -> model.tmpWaitSave
                        _ -> True
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
                            UpdateTaskPreview _ -> Just {oldTask | tmpPreview = not oldTask.tmpPreview}
                            RemoveTask _ -> Just {oldTask | mode = ModeRemove}
                            _ -> Just oldTask
                    Nothing -> Nothing
                newTasks = Dict.update taskId updateTask model.base.tasks
                oldBase = model.base
            in
            ({model | base = {oldBase | tasks = newTasks}, tmpWaitSave = doINeedSaving}, Cmd.none)
        Nothing -> (model, Cmd.none)

-- UPDATE

type Msg
    = LoadProject ProjectInfo (Result Http.Error ProjectBase)
    | Retry ProjectError
    | AskEdit
    | CancelEdit
    | UpdateEditName String
    | UpdateEditDesc String
    | UpdateEditPreview
    | SaveEdit
    | ValidStep1Edit (Result Http.Error (List ProjectInfo))
    | ValidStep2Edit (Result Http.Error String)
    | ValidStep3Edit (Result Http.Error String)
    | AddTask TaskId
    | OpenTask TaskId
    | CancelTask TaskId
    | ValidTask TaskId
    | EditTask TaskId
    | UpdateTaskTitle TaskId String
    | UpdateTaskDesc TaskId String
    | UpdateTaskPreview TaskId
    | UpdateTaskStatus TaskId String
    | DnDMsg DnDList.Msg
    | AddMoveTask TaskId
    | RemoveMoveTask TaskId
    | ToggleDnDManager
    | CancelMove
    | MoveTask TaskId TaskId
    | RemoveTask TaskId
    | DeleteTask TaskId
    | GoToHome ApiToken

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadProject infoFile result ->
            case result of
                Ok projectBase ->
                    ({model | phase = Viewing, base = projectBase , tmpDesc = projectBase.desc}, Cmd.none)
                Err error -> handleError model model.phase error
        Retry error ->
            case error.phase of
                Loading projectInfo ->
                    ( {model | phase = Loading projectInfo}
                    , readFile (FSRead projectInfo.fileId) (LoadProject projectInfo) decodeProject model.apiToken.accessToken)
                Saving step ->
                    if step == 1 then
                        ( {model | phase = Saving 1}
                        , readFile (FSRead model.homeId) ValidStep1Edit decodeHome model.apiToken.accessToken)
                    else
                        let
                            oldBase = model.base
                            newBase = {oldBase | desc = model.tmpDesc}
                            projectValue = encodeProject newBase
                        in
                        ( {model | phase = Saving 3}
                        , updateFile (FSUpdate model.projectInfo.fileId projectValue) ValidStep3Edit model.apiToken.accessToken)
                _ -> (model, Cmd.none)
        AskEdit -> ({model | phase = Editing}, Cmd.none)
        CancelEdit ->
            ({model | phase = Viewing, tmpName = model.projectInfo.name, tmpDesc = model.base.desc}, Cmd.none)
        UpdateEditName value -> ({model | tmpName = value}, Cmd.none)
        UpdateEditDesc value -> ({model | tmpDesc = value}, Cmd.none)
        UpdateEditPreview -> ({model | tmpPreview = not model.tmpPreview}, Cmd.none)
        SaveEdit ->
            ( {model | phase = Saving 1}
            , readFile (FSRead model.homeId) ValidStep1Edit decodeHome model.apiToken.accessToken)
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
                oldBase = model.base
                oldProjectInfo = model.projectInfo
                oldTopIndicators = oldProjectInfo.indicators
                newTask = JsonData.generateTask True ("Nouvelle tâche "++(String.fromInt oldBase.nextId)) parentId
                currentAddedTasks = Dict.insert (String.fromInt oldBase.nextId) newTask oldBase.tasks
                currentModel = {model | tmpWaitSave = True}
                currentBase = {oldBase | nextId = (oldBase.nextId + 1)}
            in
            case parentId of
                "-1" ->
                    let
                        newTopLevel = List.append oldBase.topLevel [String.fromInt oldBase.nextId]
                        newProjectInfo = {oldProjectInfo | indicators = {oldTopIndicators | wait = oldTopIndicators.wait + 1}}
                        newBase = {currentBase | topLevel = newTopLevel, tasks = currentAddedTasks}
                    in
                    ({currentModel | base = newBase, projectInfo = newProjectInfo}, Cmd.none)
                _ ->
                    let
                        parentTaskIds = findParentIds currentAddedTasks parentId []
                        firstParentAddedTasks =
                            let
                                firstParentUpdate maybeParentTask =
                                    case maybeParentTask of
                                        Just parentTask ->
                                            let newSubList = List.append parentTask.subTasks [(String.fromInt oldBase.nextId)] in
                                            Just {parentTask | subTasks = newSubList}
                                        Nothing -> Nothing
                            in
                            Dict.update parentId firstParentUpdate currentAddedTasks
                        parentAddedTasks = List.foldr calculateParentUpdate firstParentAddedTasks parentTaskIds
                        newTopIndicators = calculateSubTasksIndicators model.base.topLevel parentAddedTasks
                        newProjectInfo = {oldProjectInfo | indicators = newTopIndicators}
                        newBase = {currentBase | tasks = parentAddedTasks}
                    in
                    ({currentModel | base = newBase, projectInfo = newProjectInfo}, Cmd.none)
        OpenTask taskId -> handleUpdateTask taskId model msg
        CancelTask taskId -> handleUpdateTask taskId model msg
        ValidTask taskId -> handleUpdateTask taskId model msg
        EditTask taskId -> handleUpdateTask taskId model msg
        UpdateTaskTitle taskId newTitle -> handleUpdateTask taskId model msg
        UpdateTaskDesc taskId newDesc -> handleUpdateTask taskId model msg
        UpdateTaskPreview taskId -> handleUpdateTask taskId model msg
        UpdateTaskStatus taskId newStatusString ->
            case Dict.get taskId model.base.tasks of
                Just task ->
                    let
                        newStatus = case newStatusString of
                            "planned" -> Planned
                            "wip" -> Wip
                            "closed" -> Closed
                            _ -> task.status
                        newIndicators oldIndicators =
                            let sumIndicators = oldIndicators.wait + oldIndicators.wip + oldIndicators.done in
                            case newStatus of
                                Planned -> ProjectIndicators sumIndicators 0 0
                                Wip -> ProjectIndicators 0 sumIndicators 0
                                Closed -> ProjectIndicators 0 0 sumIndicators
                        -- update sub tasks
                        allSubTaskIds = case task.subTasks of
                            head::tail -> findSubTaskIds model.base.tasks head tail []
                            _ -> []
                        doSubUpdate subTaskId oldAccTasks =
                            let
                                updateSubStatus maybeSubTask =
                                    Maybe.andThen (\t -> Just {t | status = newStatus, indicators = newIndicators t.indicators}) maybeSubTask
                            in
                            Dict.update subTaskId updateSubStatus oldAccTasks
                        subUpdatedTasks = List.foldl doSubUpdate model.base.tasks allSubTaskIds
                        -- update current task
                        doCurrentUpdate maybeCurrentTask =
                            Maybe.andThen (\t -> Just {t | indicators = newIndicators t.indicators, status = newStatus}) maybeCurrentTask
                        currentUpdatedTasks = Dict.update taskId doCurrentUpdate subUpdatedTasks
                        -- update parents tasks
                        parentIds = findParentIds currentUpdatedTasks task.parentId []
                        parentUpdatedTasks = List.foldr calculateParentUpdate currentUpdatedTasks parentIds
                        -- last part
                        oldBase = model.base
                        oldProjectInfo = model.projectInfo
                        finalProjectInfo =
                            {oldProjectInfo | indicators = calculateSubTasksIndicators oldBase.topLevel parentUpdatedTasks }
                    in
                    ({model | projectInfo = finalProjectInfo, base = {oldBase | tasks = parentUpdatedTasks}, tmpWaitSave = True}, Cmd.none)
                Nothing -> (model, Cmd.none)
        DnDMsg dndMsg ->
            let (dnd, dndTasks) = dndSystem.update dndMsg model.dnd model.dndTasks in
            ({model | dnd = dnd, dndTasks = dndTasks}, dndSystem.commands model.dnd)
        AddMoveTask taskId ->
            case Dict.get taskId model.base.tasks of
                Just task ->
                    let
                        dndTasks = List.append model.dndTasks [taskId]
                        doMoveUpdate maybeTask = Maybe.andThen (\t -> Just {t | moved = True}) maybeTask
                        updatedTasks = Dict.update taskId doMoveUpdate model.base.tasks
                        oldBase = model.base
                        newBase = {oldBase | tasks = updatedTasks}
                    in
                    ({model | dndTasks = dndTasks, base = newBase}, Cmd.none)
                Nothing -> (model, Cmd.none)
        RemoveMoveTask taskId ->
            let
                dndTasks = List.filter (\n -> n /= taskId) model.dndTasks
                dndShow = if List.isEmpty dndTasks then False else model.dndShow
                doMoveUpdate maybeTask = Maybe.andThen (\t -> Just {t | moved = False}) maybeTask
                updatedTasks = Dict.update taskId doMoveUpdate model.base.tasks
                oldBase = model.base
                newBase = {oldBase | tasks = updatedTasks}
            in
            ({model | dndTasks = dndTasks, dndShow = dndShow, base = newBase}, Cmd.none)
        ToggleDnDManager -> ({model | dndShow = not model.dndShow}, Cmd.none)
        CancelMove ->
            let
                doCancelUpdate maybeTask = Maybe.andThen (\t -> Just {t | moved = False}) maybeTask
                newTasks = List.foldl (\n a -> Dict.update n doCancelUpdate a) model.base.tasks model.dndTasks
                oldBase = model.base
                newBase = {oldBase | tasks = newTasks}
            in
            ({model | dndTasks = [], dndShow = False, base = newBase}, Cmd.none)
        MoveTask parentId beforeId ->
            let
                -- deconnect all the moving tasks from their old parents
                doSeparate taskId (accTopLevel, accTasks) =
                    case Dict.get taskId accTasks of
                        Just task ->
                            if task.parentId == "-1" then (List.filter (\n -> n /= taskId) accTopLevel, accTasks)
                            else
                                let
                                    parentTaskIds = findParentIds accTasks task.parentId []
                                    doUpdateParent maybeParentTask =
                                        Maybe.andThen (\t -> Just {t | subTasks = List.filter (\n -> n /= taskId) t.subTasks}) maybeParentTask
                                    firstParentUpdate = Dict.update task.parentId doUpdateParent accTasks
                                    updatedParentTasks = List.foldr calculateParentUpdate firstParentUpdate parentTaskIds
                                in
                                (accTopLevel, updatedParentTasks)
                        Nothing -> (accTopLevel, accTasks)
                (updatedTopLevel, updatedTasks) = List.foldl doSeparate (model.base.topLevel, model.base.tasks) model.dndTasks
                -- reconnect the moving tasks to their new parent
                recurOldList beforeList afterList =
                    case afterList of
                        x::xs ->
                            if x == beforeId then List.concat [beforeList, model.dndTasks, afterList]
                            else recurOldList (List.append beforeList [x]) xs
                        [] -> List.append beforeList model.dndTasks
                updateOldList : List String -> List String
                updateOldList oldList =
                    if beforeId == "foot" then List.append oldList model.dndTasks
                    else recurOldList [] oldList
                (reconnectedTopLevel, reconnectedTasks) =
                    if parentId == "-1" then (updateOldList updatedTopLevel, updatedTasks)
                    else
                        let
                            parentTaskIds = findParentIds updatedTasks parentId []
                            doUpdateParent maybeParentTask =
                                Maybe.andThen (\t -> Just {t | subTasks = updateOldList t.subTasks}) maybeParentTask
                            firstParentUpdate = Dict.update parentId doUpdateParent updatedTasks
                            updatedParentTasks = List.foldr calculateParentUpdate firstParentUpdate parentTaskIds
                        in
                        (updatedTopLevel, updatedParentTasks)
                -- update the moved tasks
                doMovedUpdate maybeTask =
                    Maybe.andThen (\t -> Just {t | moved = False, parentId = parentId}) maybeTask
                finalTasks = List.foldl (\n a -> Dict.update n doMovedUpdate a) reconnectedTasks model.dndTasks
                -- update the model
                oldBase = model.base
                oldProjectInfo = model.projectInfo
                newBase = {oldBase | tasks = finalTasks, topLevel = reconnectedTopLevel}
                newTopIndicators = calculateSubTasksIndicators reconnectedTopLevel finalTasks
                newProjectInfo = {oldProjectInfo | indicators = newTopIndicators}
                newModel = {model | base = newBase, projectInfo = newProjectInfo, dndTasks = [], dndShow = False, tmpWaitSave = True}
            in
            (newModel, Cmd.none)
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
                        subPurgedTasks = List.foldl doPurge model.base.tasks allSubTaskIds
                        -- remove currentTask
                        currentPurgedTasks = Dict.remove taskId subPurgedTasks
                        -- recalculate parent indicators
                        parentTaskIds = findParentIds currentPurgedTasks task.parentId []
                        doParentPurge parentId accTasks =
                            let
                                updatePurgedParent maybeParentTask =
                                    Maybe.andThen
                                        (\t ->
                                            let
                                                newSubTasks = List.filter (\n -> n /= taskId) t.subTasks
                                                newParentIndicators = calculateSubTasksIndicators newSubTasks accTasks
                                                newParentStatus =
                                                    if newParentIndicators.wip == 0 then
                                                        if newParentIndicators.done == 0 then Planned
                                                        else if newParentIndicators.wait == 0 then Closed
                                                        else Wip
                                                    else Wip
                                            in
                                            Just {t | indicators = newParentIndicators, status = newParentStatus, subTasks = newSubTasks})
                                        maybeParentTask
                            in
                            Dict.update parentId updatePurgedParent accTasks
                        parentPurgedTasks = List.foldr doParentPurge currentPurgedTasks parentTaskIds
                        -- final part
                        oldProjectInfo = model.projectInfo
                        oldBase = model.base
                        newProjectIndicators = calculateSubTasksIndicators oldBase.topLevel parentPurgedTasks
                        newProjectInfo = {oldProjectInfo | indicators = newProjectIndicators}
                    in
                    ({model | projectInfo = newProjectInfo, base = {oldBase | tasks = parentPurgedTasks}, tmpWaitSave = True}, Cmd.none)
                Nothing -> (model, Cmd.none)
        GoToHome _ -> (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    dndSystem.subscriptions model.dnd

-- VIEW

view : Model -> Html Msg
view model =
    div [class "core"]
        (List.append
            [ div [class "zone_status"]
                (case model.phase of
                    Loading infoFile -> [span [] [text ("Projet : "++infoFile.name)]]
                    Failing error ->
                        [ span [class "error"] [text "Il y a eu un problème !"]
                        , button [onClick (Retry error), class "button_topsnap"] [text "Réessayer"]
                        ]
                    Viewing ->
                        [ span [] [text model.projectInfo.name]
                        , button [onClick (GoToHome model.apiToken), class "button_topsnap"] [iconClose]
                        , button [onClick AskEdit, class "button_topsnap"] [iconEdit]
                        ]
                    Editing -> [span [] [text model.projectInfo.name]]
                    Saving _ -> [span [] [text "Mise à jour du projet"]]
                )
            , (case model.phase of
                Viewing ->
                    div [class "project_tracker indicators_tracker"] (List.append
                        (viewIndicatorsTracker model.projectInfo.indicators "ce projet")
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
                Viewing->
                    div []
                        [ (section [class "project_desc"] [
                            (if (String.isEmpty (String.trim model.base.desc)) then text "(Description absente)"
                            else Markdown.toHtml [] model.base.desc)])
                        , hr [] []
                        , section []
                            (viewTasks (not (List.isEmpty model.dndTasks)) "-1" model.base.tasks model.base.topLevel)
                        ]
                Editing ->
                    let previewText = if model.tmpPreview then "Édition" else "Preview" in
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
                        , p []
                            [ label [for "project_desc"] [text "Description :"]
                            , button [onClick UpdateEditPreview, class "button"] [text previewText]
                            ]
                        ,
                            (if model.tmpPreview then Markdown.toHtml [] model.tmpDesc
                            else textarea [id "project_desc", onInput UpdateEditDesc, rows 11] [text model.tmpDesc])
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
            )]
            (if List.isEmpty model.dndTasks then [Html.nothing] else viewDnDManager model)
        )

viewIndicatorsTracker : ProjectIndicators -> String -> List (Html Msg)
viewIndicatorsTracker indicators endSentence =
    let
        total = indicators.wait + indicators.wip + indicators.done
    in
    if total > 0 then
        if total == indicators.done then
            [ span
                [class "project_end"]
                [text ("Vous avez terminé "++endSentence++" ! (soit "++(String.fromInt total)++" tâches)")]
            ]
        else
            [ progressBar indicators
            , div [class "indicators_progress"]
                [ span [class "round_box wait_color"] [text (String.fromInt indicators.wait)]
                , span [class "round_box wip_color"] [text (String.fromInt indicators.wip)]
                , span [class "round_box done_color"] [text (String.fromInt indicators.done)]
                , span [class "round_box total_color"] [text (String.fromInt total)]
                ]
            ]
    else [Html.nothing]

viewTasks : Bool -> TaskId -> Dict String ProjectTask -> List String -> List (Html Msg)
viewTasks isDragIn parentId tasks taskIds =
    List.map (viewTask isDragIn tasks) taskIds
        |> List.concat |> (\n -> List.append n [viewPlaceholder isDragIn parentId "foot"])

viewTask : Bool -> Dict String ProjectTask -> String -> List (Html Msg)
viewTask isDragIn tasks taskId =
    case Dict.get taskId tasks of
        Just task ->
            let
                transientDragIn = if task.moved then False else isDragIn
                statusColor = case task.status of
                    Planned -> "wait_color"
                    Wip -> "wip_color"
                    Closed -> "done_color"
                taskClasses = "task_bar round_box "++statusColor
                statusSelector =
                    select [class statusColor, onInput (UpdateTaskStatus taskId), class "task_select round_box"]
                        [ option [class "wait_color", value "planned", selected (task.status == Planned)] [text "Attente"]
                        , option [class "wip_color", value "wip", selected (task.status == Wip)] [text "En cours"]
                        , option [class "done_color", value "closed", selected (task.status == Closed)] [text "Terminée"]
                        ]
                (actions, buttons, content) = case task.mode of
                    ModeEdit ->
                        let previewText = if task.tmpPreview then "Édition" else "Preview" in
                        ( [class taskClasses]
                        ,
                            [ button [onClick (ValidTask taskId), class "button_round"] [iconValid]
                            , button [onClick (CancelTask taskId), class "button_round"] [iconClose]
                            , statusSelector
                            ]
                        ,
                            [ ( if task.tmpPreview then Markdown.toHtml [class "task_desc"] task.tmpDesc
                                else textarea [onInput (UpdateTaskDesc taskId), rows 9] [text task.tmpDesc])
                            , p [] [button [onClick (UpdateTaskPreview taskId), class "button"] [text previewText]]
                            ]
                        )
                    ModeView ->
                        let
                            moveAttrs =
                                if task.moved then [onClick (RemoveMoveTask taskId), class "project_dnd_button_round"]
                                else [onClick (AddMoveTask taskId), class "button_round"]
                        in
                        ( (class taskClasses)::[onClick (OpenTask taskId)]
                        ,
                            [ button [onClick (RemoveTask taskId), class "button_round"] [iconClose]
                            , button [onClick (EditTask taskId), class "button_round"] [iconEdit]
                            , button moveAttrs [iconMove]
                            , statusSelector
                            ]
                        , case task.opened of
                            True ->
                                let
                                    subTasksList =
                                        if List.isEmpty task.subTasks then [viewPlaceholder transientDragIn taskId "foot", hr [] []]
                                        else [div [class "task_subtasks"] (viewTasks transientDragIn taskId tasks task.subTasks)]
                                in
                                List.concat
                                    [ [article [class "task_desc"] [Markdown.toHtml [] task.desc]]
                                    , (
                                        if List.isEmpty task.subTasks then [Html.nothing]
                                        else [div [] [text "Liste des sous-tâches :"]]
                                    )
                                    , [ div [class "indicators_tracker"]
                                        (List.append
                                            (if List.isEmpty task.subTasks then [Html.nothing]
                                            else viewIndicatorsTracker task.indicators "les sous tâches")
                                            [button [class "button_bottomsnap", onClick (AddTask taskId)] [iconAdd]]
                                            )
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
            [ viewPlaceholder isDragIn task.parentId taskId
            , div [] (List.append
                [div [class "task_line"]
                    ((div actions
                        [case task.mode of
                            ModeEdit ->
                                input
                                    [ value task.tmpTitle
                                    , onInput (UpdateTaskTitle taskId)
                                    , maxlength 30
                                    , size 32
                                    ] []
                            _ ->
                                let
                                    sub =
                                        if task.indicators.wait > 0
                                        || task.indicators.wip > 0
                                        || task.indicators.done > 0
                                        then " ↡ " else ""
                                in
                                text (sub++task.title)
                        ])::buttons)
                ]
                content
                )
            ]
        Nothing -> [Html.nothing]    

viewPlaceholder : Bool -> TaskId -> TaskId -> Html Msg
viewPlaceholder isDragIn parentId beforeId =
    div
        ([class (if isDragIn then "task_placeholder" else "task_space")]
        ++(if isDragIn then [onClick (MoveTask parentId beforeId)] else []))
        []

viewDnDManager : Model -> List (Html Msg)
viewDnDManager model =
    let
        managerToggle = button
            [ class "button_rightsnap"
            , onClick ToggleDnDManager
            , style "top" "100px"
            , style "right" (if model.dndShow then "306px" else "0")
            ]
            [iconMove]
        managerCancel = button
            [ class "button_rightsnap"
            , onClick CancelMove
            , style "top" "130px"
            , style "right" (if model.dndShow then "306px" else "0")
            ]
            [iconClose]
    in
    managerToggle::managerCancel
    ::(if model.dndShow then
        section [class "project_dnd_manager"]
            (List.indexedMap (viewDnDTask (dndSystem.info model.dnd) model.base.tasks) model.dndTasks)
        else Html.nothing
    )::[]

viewDnDTask : Maybe DnDList.Info -> Dict TaskId ProjectTask -> Int -> DnDTask -> Html Msg
viewDnDTask mInfo tasks index dndTask =
    let
        dndId = "id-"++dndTask
        taskTitle = case Dict.get dndTask tasks of
            Just {title} -> title
            Nothing -> ""
    in
    case mInfo of
        Just {dragIndex} ->
            if dragIndex /= index then
                div
                    ([id dndId, class "task_line with_margins"]++dndSystem.dropEvents index dndId)
                    [div [class "project_dnd_taskbar dnd_color"] [text taskTitle]]
            else
                div [class "task_line round_box dnd_color", style "width" "260px"] []
        Nothing ->
            div
                [id dndId, class "task_line with_margins"]
                [ span
                    (class "project_dnd_taskbar dnd_color"::dndSystem.dragEvents index dndId)
                    [text taskTitle]
                , button
                    [onClick (RemoveMoveTask dndTask), class "project_dnd_button_round"]
                    [iconClose]
                ]
