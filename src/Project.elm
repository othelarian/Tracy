module Project exposing(Model, Msg(..), createNewProject, init, update, view)

import Api exposing (..)
import Graphics exposing (..)
import JsonData exposing (..)
import TracyTask

import Dict exposing (Dict)
import Html exposing (Html, button, div, hr, input, label, p, span, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Extra as Html
import Http
import Json.Decode as JD
import Json.Decode exposing (Decoder, field, int, list, string)
import Json.Encode as JE
import Tuple

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
    --
    -- TODO : viewing va abriter toute les modifications des tâches
    --
    | Viewing
    --
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
    , tasks : Dict Int TracyTask.Model
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
        (ProjectBase "" 0 [])
        Dict.empty
        ""
        testShow

-- HANDLERS

createNewProject : String -> ProjectBase
createNewProject desc =
    ProjectBase desc 0 []

handleError : Model -> ProjectPhase -> Http.Error -> (Model, Cmd Msg)
handleError model phase error =
    ({model | phase = Failing (ProjectError phase (httpErrorToString error))}, Cmd.none)

handleUpdateHome : Model -> List ProjectInfo -> (Model, Cmd Msg)
handleUpdateHome model projects =
    let
        letChangeProject projectInfo =
            if projectInfo.fileId == model.projectInfo.fileId then
                {projectInfo | name = model.tmpName, values = model.projectInfo.values}
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
    | AddTask Int
    | ValidTask
    | EditTask
    | UpdateTaskTitle String
    | MoveTask
    | RemoveTask
    | GoToHome ApiCredentials

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadProject infoFile result ->
            case result of
                Ok projectBase ->
                    let newDictTasks = TracyTask.populate projectBase.tasks -1 in
                    ( {model | phase = Viewing , base = projectBase , tmpDesc = projectBase.desc, tasks = newDictTasks}
                    , Cmd.none)
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
                --
                -- TODO : prendre en compte l'ordre
                --
                --
                newTask = ProjectTask "Nouvelle tâche" oldBase.nextId parentId 0 JsonData.Planned ""
                oldBase = model.base
                newBase = {oldBase | nextId = (oldBase.nextId + 1), tasks = (newTask::oldBase.tasks)}
                newValues = case model.projectInfo.values of
                    v1::tail -> (v1+1)::tail
                    _ -> [1,0,0]
                oldProjectInfo = model.projectInfo
                newProjectInfo = {oldProjectInfo | values = newValues}
                newDictTasks = Dict.insert oldBase.nextId (TracyTask.generate newTask) model.tasks
            in
            ({model | base = newBase, projectInfo = newProjectInfo, tasks = newDictTasks, tmpWaitSave = True} , Cmd.none)
        ValidTask ->
            --
            -- TODO : plus compliqué que prévu
            --
            (model, Cmd.none)
            --
        EditTask ->
            --
            --
            --
            (model, Cmd.none)
            --
        UpdateTaskTitle newTitle ->
            --
            --
            --
            (model, Cmd.none)
            --
        MoveTask ->
            --
            --
            (model, Cmd.none)
            --
        RemoveTask ->
            --
            --
            --
            (model, Cmd.none)
            --
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
            Loading _ -> div [class "waiter"] [text "Chargement du projet en cours, veuillez patienter"]
            Failing error -> div [class "error"] [text error.info]
            Viewing ->
                div []
                    [ div []
                        --
                        [],
                        --
                        (
                        --
                        -- TODO : au lieu d'avoir seulement le bouton de sauvegarde, il faudrait également les indicateurs
                        --
                        --
                        if model.tmpWaitSave then
                            p [class "centered"] [button [class "button", onClick SaveEdit] [text "Sauvegarder"]]
                        else Html.nothing
                        )
                    , ( if (String.isEmpty (String.trim model.base.desc)) then div [] [text "(Description absente)"]
                        else div [] [text model.base.desc]
                        )
                    , hr [] []
                    , div [] (viewTasks model.tasks -1)
                    , p [class "centered"] [button [class "button", onClick (AddTask -1)] [text "Ajouter une tâche"]]
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
                            , style "width" "170px"
                            ]
                            []
                        ]
                    , p [] [label [for "project_desc"] [text "Description :"]]
                    , textarea
                        [id "project_desc", onInput UpdateEditDesc, style "width" "90%", rows 7]
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

viewTasks : Dict Int TracyTask.Model -> Int -> List (Html Msg)
viewTasks tasks parentId =
    let filteredTasks = Dict.filter (\_ n -> n.parent == parentId) tasks in
    --
    -- TODO : pas sûr que ça soit suffisant, à vérifier
    --
    --List.map (viewTask tasks) filteredTasks
    Dict.foldl viewTask [] filteredTasks
    --

viewTask : Int -> TracyTask.Model ->  -> Html Msg
viewTask taskId model =
    --
    --
    Html.nothing
    --
