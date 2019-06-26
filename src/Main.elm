port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Url

-- TODO : connexion aux drive APIs
-- TODO : listing des projets
-- TODO : page d'accueil d'un projet
-- TODO : page de listing des actions à mener

-- MAIN

main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }

-- MODEL

type Status
    = Starting
    | LogIn
    | Home
    | Project

type Question
    = AskInit
    | AskLogIn
    | AskLogOut

type alias Model =
    { url : Url.Url
    , key : Nav.Key
    , status : Status
    }

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
    (Model url key Starting, ask "Starting")

-- UPDATE

type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | Asking Question
    | ReceptionData String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    (model, Nav.pushUrl model.key (Url.toString url))
                Browser.External href ->
                    (model, Nav.load href)
        UrlChanged url ->
            ({model | url = url}, Cmd.none)
        Asking question ->
            --
            -- TODO : comment faire la demande ?
            --
            (model, ask "")
        ReceptionData value ->
            --
            -- TODO : traitement sur les données
            --
            (model, Cmd.none)

-- PORTS

port ask : String -> Cmd msg
port received : (String -> msg) -> Sub msg

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
    received ReceptionData

-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "Tracy"
    , body = [
        h1 [] [text "Tracy"]
        , div [class "core"] [(case model.status of
            Starting -> div [] [text "Connexion en cours ..."]
            LogIn -> div [] []
            Home -> div [] []
            Project -> div [] []
        )]
        ]
    }


