module Project exposing(Model, Msg(..), createNewProject, init, update, view)

import Api exposing (..)
import Graphics exposing (..)
import JsonData exposing (..)

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
                    , apiReadFile (FSRead model.homeId) ValidStep1EditName JsonData.decodeHome model.apiCredentials)
                --
                -- TODO : comme pour le Home, on doit pouvoir réessayer de passer les commandes
                --
                _ -> (model, Cmd.none)
        AskEditName -> ({model | phase = NameEditing, tmpName = model.infoFile.name}, Cmd.none)
        CancelEditName -> ({model | phase = Viewing}, Cmd.none)
        UpdateEditName value -> ({model | tmpName = value}, Cmd.none)
        SaveEditName ->
            ( {model | phase = NameSaving 1}
            , apiReadFile (FSRead model.homeId) ValidStep1EditName JsonData.decodeHome model.apiCredentials)
        ValidStep1EditName result ->
            case result of
                Ok projects ->
                    let
                        letChangeName infoFile =
                            if infoFile.fileId == model.infoFile.fileId then
                                {infoFile | name = model.tmpName}
                            else infoFile
                        modProjects = List.map letChangeName projects
                        homeValue = JsonData.encodeHome modProjects
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
                    , button [onClick (GoToHome model.apiCredentials), class "button_topsnap"] [iconClose]
                    , button [onClick AskEditName, class "button_topsnap"] [iconEdit]
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
                if (String.isEmpty (String.trim model.base.desc)) then div [] [text "(Description absente)"]
                else div [] [text model.base.desc]
                --
                -- TODO : affichage des données globales
                --
                -- TODO : affichage de la liste des tâches
                --
                --
            NameEditing ->
                p []
                    [ span [] [label [for "project_name"] [text "Nom du projet : "]]
                    , span [style "display" "inline-block"]
                        [ input
                            [ id "project_name"
                            , value model.tmpName
                            , onInput UpdateEditName
                            , maxlength 20
                            , style "width" "170px"
                            ]
                            []
                        , button [onClick CancelEditName, class "button_round"] [iconClose]
                        , button [onClick SaveEditName, class "button_round"] [iconValid]
                        ]
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
