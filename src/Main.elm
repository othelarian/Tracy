port module Main exposing (..)

import Api exposing (..)
import Home
import Project

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, h1, input, p, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Decode exposing (Decoder, array, decodeValue, errorToString, field, int, string)
import Json.Encode as JE


-- WIP : listing des projets
-- WIP : ajouter des projets
-- TODO : supprimer des projets

-- TODO : page d'accueil d'un projet, avec son nom et sa description
-- TODO : faire en sorte de pouvoir modifier le nom et la description

-- TODO : page de listing des actions à mener

-- MAIN

main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- MODEL

type GuestPhase
    = Connecting
    | Login (Maybe String)
    | Failed String

type alias GuestModel =
    { apiCredentials : ApiCredentials
    , phase : GuestPhase
    --
    --
    , testValue : String
    --
    }

guestModel : ApiCredentials -> GuestModel
guestModel apiCredentials =
    GuestModel apiCredentials (Login Nothing) ""

type Model
    = Guest GuestModel
    | Home Home.Model
    | Project Project.Model

init : ApiKey -> (Model, Cmd Msg)
init apiKey =
    let
        credentials = ApiCredentials apiKey ""
        model = guestModel credentials
    in
    (Guest {model | phase = Connecting}, ask "Starting")

-- JSON DECODE

type Answer
    = AError String
    | Connected (Result JD.Error Token)
    | Unconnected

decodeAnswer : JE.Value -> Answer
decodeAnswer value =
    let
        checkStatus = decodeValue (field "status" string) value
    in
    case checkStatus of
        Err error -> AError ("checkStatus error: "++(errorToString error))
        Ok status ->
            case status of
                "AError" ->
                    let
                        checkDesc = decodeValue (field "desc" string) value
                    in
                    case checkDesc of
                        Err error -> AError ("checkDesc error: "++(errorToString error))
                        Ok desc -> AError desc
                "Connected" -> Connected (decodeValue (field "token" string) value)
                "Unconnected" -> Unconnected
                _ -> AError "Something went wrong, bad value for status"

-- UPDATE

type Question
    = AskLogIn
    | AskLogOut

type Msg
    = Asking Question
    | ReceptionData JE.Value
    | HomeMsg Home.Msg
    | ProjectMsg Project.Msg
    --
    -- ############
    --
    | TestList
    | TestCreate
    | TestRead
    | TestDelete
    | TestRepList (Result Http.Error (Array InfoFile))
    | TestRepString (Result Http.Error String)
    | TestRepValue (Result Http.Error JD.Value)
    | TestChange String
    --

updateWith : (subModel -> Model) -> (subMsg -> Msg) -> (subModel, Cmd subMsg) -> (Model, Cmd Msg)
updateWith toModel toMsg (subModel, subCmd) =
    (toModel subModel, Cmd.map toMsg subCmd)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model) of
        (Asking question, Guest guest) ->
            case question of
                AskLogIn -> (Guest {guest | phase = Connecting}, ask "Authenticate")
                AskLogOut -> (Guest guest, ask "SignOut")
        (ReceptionData value, _) ->
            let
                answer = decodeAnswer value
                credentials = case model of
                    Guest guest -> guest.apiCredentials
                    Home home -> home.apiCredentials
                    Project project -> project.apiCredentials
            in
            case answer of
                AError info ->
                    let
                        newGuest = guestModel credentials
                    in
                    (Guest {newGuest | phase = Failed info}, Cmd.none)
                Unconnected ->
                    let
                        newCredentials = ApiCredentials credentials.apiKey ""
                    in
                    (Guest (guestModel newCredentials), Cmd.none)
                Connected resToken ->
                    case resToken of
                        Err error ->
                            let
                                newGuest = guestModel credentials
                            in
                            (Guest {newGuest | phase = Failed (errorToString error)}, Cmd.none)
                        Ok token ->
                            let
                                newCredentials = ApiCredentials credentials.apiKey token
                            in
                            ( Home (Home.init newCredentials)
                            , Cmd.map HomeMsg (apiGetListFiles FSNone Home.Check newCredentials))
        (HomeMsg subMsg, Home home) ->
            updateWith Home HomeMsg (Home.update subMsg home)
        (ProjectMsg subMsg, Project project) ->
            updateWith Project ProjectMsg (Project.update subMsg project)
        --
        -- TODO : après cette ligne ce trouve des fonctions de tests qui devraient disparaître à terme
        --
        (TestList, _) ->
            let (creds, _) = fromModelTest model in (model, apiGetListFiles FSNone TestRepList creds)
        (TestCreate, _) ->
            let
                testObject = JE.object
                    [ ("key1", JE.string "value1")
                    , ("key2", JE.string "value2")
                    ]
                (creds, name) = fromModelTest model
            in
            (model, apiCreateFile (FSCreate name testObject) TestRepString creds)
        (TestRead, _) ->
            let (creds, fileId) = fromModelTest model in
            (model, apiReadFile (FSRead fileId) TestRepString decodeTestRead creds)
        (TestDelete, _) ->
            let (creds, fileId) = fromModelTest model in
            (model, apiDeleteFile (FSDelete fileId) TestRepValue creds)
        (TestRepList rep, _) -> (model, Cmd.none)
        (TestRepString rep, _) -> (model, Cmd.none)
        (TestRepValue rep, _) -> (model, Cmd.none)
        (TestChange value, _) ->
            case model of
                Guest data -> (Guest {data | testValue = value}, Cmd.none)
                Home data -> (Home {data | testValue = value}, Cmd.none)
                Project data -> (Project {data | testValue = value}, Cmd.none)
        --
        --
        (_, _) -> (model, Cmd.none)

-- PORTS

port ask : String -> Cmd msg
port received : (JE.Value -> msg) -> Sub msg

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
    received ReceptionData

-- VIEW

view : Model -> Browser.Document Msg
view model =
    let
        getDecoBtn : List (Html Msg)
        getDecoBtn =
            [div [class "panel_deconnect"] [button
                [onClick (Asking AskLogOut), class "button_topsnap"]
                [text "Déconnexion"]
                ]]
        decoBtn = case model of
            Guest guest ->
                case guest.phase of
                    Login _ -> []
                    Connecting -> []
                    _ -> getDecoBtn
            _ -> getDecoBtn
    in
    { title = "Tracy"
    , body = List.concat
        [ decoBtn
        , [ case model of
                Guest guest ->
                    case guest.phase of
                        Connecting -> div [class "core waiter"] [text "Connexion en cours ..."]
                        Login maybe ->
                            div [class "panel_connect"]
                                [ button [onClick (Asking AskLogIn), class "button"] [text "Connexion"]
                                , case maybe of
                                    Just error -> div [class "error"] [text error]
                                    Nothing -> div [class "hide"] []
                                ]
                        Failed error ->
                            div [class "core error"]
                                [ p [] [text "Vous êtes bien connecté, mais il y a eu une erreur lors du retour."]
                                , p [] [text "L'erreur remontée est la suivante :"]
                                , div [] [text error]
                                ]
                Home home -> Html.map HomeMsg (Home.view home)
                Project project -> Html.map ProjectMsg (Project.view project)
        ]
        --
        -- TODO : div avec tous les éléments pour les tests
        --
        , [
            div [] [
                button [onClick TestList] [text "List"]
                , button [onClick TestRead] [text "Read"]
                , button [onClick TestCreate] [text "Create"]
                , let
                    testValue = case model of
                        Guest data -> data.testValue
                        Home data -> data.testValue
                        Project data -> data.testValue
                    in
                    input [value testValue, onInput TestChange, style "width" "250px"] []
                , button [onClick TestDelete] [text "Delete"]
            ]]
        ]
    }



-- TEST FUNCTIONS

decodeTest : Decoder (Array String)
decodeTest =
    field "files" (array string)

decodeTestRead : Decoder String
decodeTestRead =
    JD.oneOf [ field "projects" string, field "name" string]

fromModelTest : Model -> (ApiCredentials, String)
fromModelTest model =
    case model of
        Guest data -> (data.apiCredentials, data.testValue)
        Home data -> (data.apiCredentials, data.testValue)
        Project data -> (data.apiCredentials, data.testValue)
