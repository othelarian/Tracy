import Browser
import Browser.Navigation as Nav
import Html exposing (Html, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Url

import Debug

-- MAIN

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }

-- MODEL

type alias Model =
    { url : Url.Url
    , key : Nav.Key
    }

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
    (Model url key, Cmd.none)

-- HANDLERS

-- UPDATE

type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            --
            --
            (model, Cmd.none)
            --
        UrlRequested urlRequested ->
            --
            --
            (model, Cmd.none)
            --

-- SUB

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "identity endpoint test"
    , body =
        [ text "iframe"
        ]
    }

