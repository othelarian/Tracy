module Project exposing(Model, Msg(..), createNewProject, decodeProject, encodeProject, init, update, view)

import Api exposing (..)

import Html exposing (Html, button, div, input, label, p, span, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Extra as Html
import Http
import Json.Decode as JD
import Json.Decode exposing (Decoder, field, int, list, string)
import Json.Encode as JE

import Debug

-- MODEL

type alias ProjectError =
    { phase : ProjectPhase
    , info : String
    }

type alias Step = Int

type ProjectPhase
    = Loading InfoFile
    | Failing ProjectError
    | Viewing
    | NameEditing
    | NameSaving Step
    --
    --| DescEditing
    --

type ProjectTaskStatus
    = Planned
    | Wip
    | Closed

type alias ProjectTask =
    { title : String
    , id : Int
    , parent : Int
    , status : ProjectTaskStatus
    , desc : String
    }

type alias ProjectBase =
    { desc : String
    , lastId : Int
    , tasks : List ProjectTask
    }

type alias Model =
    { apiCredentials : ApiCredentials
    , infoFile : InfoFile
    , homeId : FileId
    , tmpName : String
    , phase : ProjectPhase
    , base : ProjectBase
    , testValue : String
    , testShow : Bool
    }

init : ApiCredentials -> InfoFile -> FileId -> Bool -> Model
init apiCredentials infoFile homeId testShow =
    Model apiCredentials infoFile homeId "" (Loading infoFile) (ProjectBase "" 0 []) "" testShow

-- HANDLERS

createNewProject : String -> ProjectBase
createNewProject desc =
    ProjectBase desc 0 []

handleError : Model -> ProjectPhase -> Http.Error -> (Model, Cmd Msg)
handleError model phase error =
    ({model | phase = Failing (ProjectError phase (httpErrorToString error))}, Cmd.none)

-- JSON DECODE

decodeProjectTask : Decoder ProjectTask
decodeProjectTask =
    let
        getProjectTaskStatus status =
            case status of
                "Planned" -> Planned
                "Wip" -> Wip
                _ -> Closed
    in
    JD.map5 ProjectTask
        (field "title" string)
        (field "id" int)
        (field "parent" int)
        (field "status" (JD.map getProjectTaskStatus string))
        (field "desc" string)

decodeProject : Decoder ProjectBase
decodeProject =
    JD.map3 ProjectBase
        (field "desc" JD.string)
        (field "lastId" int)
        (field "tasks" (list decodeProjectTask))

decodePartialHome : Decoder (List InfoFile)
decodePartialHome =
    (field "projects" (list decodeInfoFile))

-- JSON ENCODE

encodeProjectTask : ProjectTask -> JE.Value
encodeProjectTask task =
    let
        encodeProjectTaskStatus status =
            case status of
                Planned -> "Planned"
                Wip -> "Wip"
                Closed -> "Closed"
    in
    JE.object
        [ ("title", JE.string task.title)
        , ("id", JE.int task.id)
        , ("parent", JE.int task.parent)
        , ("status", JE.string (encodeProjectTaskStatus task.status))
        , ("desc", JE.string task.desc)
        ]

encodeProject : ProjectBase -> JE.Value
encodeProject base =
    JE.object
        [ ("desc", JE.string base.desc)
        , ("lastId", JE.int base.lastId)
        , ("tasks", JE.list encodeProjectTask base.tasks)
        ]

encodePartialHome : List InfoFile -> JE.Value
encodePartialHome projects =
    let
        encodeInfoFile : InfoFile -> JE.Value
        encodeInfoFile infoFile =
            JE.object
                [ ("id", JE.string infoFile.fileId)
                , ("name", JE.string infoFile.name)
                ]
    in
    JE.object [ ("projects", JE.list encodeInfoFile projects) ]

-- UPDATE

type Msg
    = LoadProject InfoFile (Result Http.Error ProjectBase)
    | Retry ProjectError
    | AskEditName
    | CancelEditName
    | UpdateEditName String
    | SaveEditName
    | ValidStep1EditName (Result Http.Error (List InfoFile))
    | ValidStep2EditName (Result Http.Error String)
    --
    --
    --| AskEditDesc
    --
    --| CancelEditDesc
    --
    --| ValidEditDesc
    --
    --
    --
    | GoToHome ApiCredentials

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadProject infoFile result ->
            case result of
                Ok projectBase -> ({model | phase = Viewing, base = projectBase}, Cmd.none)
                Err error -> handleError model model.phase error
        Retry error ->
            case error.phase of
                Loading infoFile ->
                    ( {model | phase = Loading infoFile}
                    , apiReadFile (FSRead infoFile.fileId) (LoadProject infoFile) decodeProject model.apiCredentials)
                NameSaving _ ->
                    ( {model | phase = NameSaving 1}
                    , apiReadFile (FSRead model.homeId) ValidStep1EditName decodePartialHome model.apiCredentials)
                --
                -- TODO : comme pour le Home, on doit pouvoir réessayer de passer les commandes
                --
                _ -> (model, Cmd.none)
        AskEditName -> ({model | phase = NameEditing, tmpName = model.infoFile.name}, Cmd.none)
        CancelEditName -> ({model | phase = Viewing}, Cmd.none)
        UpdateEditName value -> ({model | tmpName = value}, Cmd.none)
        SaveEditName ->
            ( {model | phase = NameSaving 1}
            , apiReadFile (FSRead model.homeId) ValidStep1EditName decodePartialHome model.apiCredentials)
        ValidStep1EditName result ->
            case result of
                Ok projects ->
                    let
                        letChangeName infoFile =
                            if infoFile.fileId == model.infoFile.fileId then
                                {infoFile | name = model.tmpName}
                            else infoFile
                        modProjects = List.map letChangeName projects
                        homeValue = encodePartialHome modProjects
                    in
                    ( {model | phase = NameSaving 2}
                    , apiUpdateFile (FSUpdate model.homeId homeValue) ValidStep2EditName model.apiCredentials)
                Err error -> handleError model model.phase error
        ValidStep2EditName result ->
            case result of
                Ok _ ->
                    let
                        tmpInfoFile = model.infoFile
                        newInfoFile = {tmpInfoFile | name = model.tmpName}
                    in
                    ({model | phase = Viewing, infoFile = newInfoFile} , Cmd.none)
                Err error -> handleError model (NameSaving 1) error
            --
        --AskEditDesc ->
        --CancelEditDesc ->
        --ValidEditDesc ->
        --
        --
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
                    [ span [] [text model.infoFile.name]
                    , button [onClick (GoToHome model.apiCredentials), class "button_topsnap"] [text "Fermer"]
                    , button [onClick AskEditName, class "button_topsnap"] [text "Renommer"]
                    ]
                NameEditing -> [span [] [text model.infoFile.name]]
                NameSaving _ -> [span [] [text "Mise à jour du nom du projet"]]
                --
                --
                --
                --_ -> [p [] [text "(en cours de construction)"]]
                --
            )
        , (case model.phase of
            Loading _ -> div [class "waiter"] [text "Chargement du projet en cours, veuillez patienter"]
            Failing error -> div [class "error"] [text error.info]
            Viewing ->
                --
                -- TODO : affichage standard du projet ici
                --
                div [] [text "(Votre projet, ici !!!!!)"]
                --
            NameEditing ->
                div []
                    [ label [for "project_name"] [text "Nom du projet : "]
                    , input
                        [ id "project_name"
                        , value model.tmpName
                        , onInput UpdateEditName
                        , maxlength 20
                        , style "width" "200px"
                        ]
                        []
                    , button [onClick CancelEditName, class "button"] [text "Annuler"]
                    , button [onClick SaveEditName, class "button"] [text "Valider"]
                    ]
            NameSaving step ->
                case step of
                    1 -> div [class "waiter"] [text "Récupération des informations ..."]
                    2 -> div [class "waiter"] [text "Sauvegarde du nouveau nom ..."]
                    _ -> Html.nothing
            --
            --
            --
            --
            --_ -> div [class "waiter"] [text "Veuillez patienter"]
        )
        ]
