import Keys

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, iframe, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Url
import Url.Builder as UB

import Debug

-- MAIN

main : Program () Model Msg
main =
    --Browser.application
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        --, onUrlChange = UrlChanged
        --, onUrlRequest = UrlRequested
        }

-- MODEL

type Phase
    = Init
    | Connecting

type alias Model =
    --{ url : Url.Url
    --, key : Nav.Key
    { phase : Phase
    }

--init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
--init _ url key =
--    (Model url key Init, Cmd.none)

init : () -> (Model, Cmd Msg)
init _ =
    (Model Init, Cmd.none)

-- HANDLERS

createIframeUrl : String
createIframeUrl =
    --
    UB.crossOrigin
        "https://accounts.google.com"
        ["o", "oauth2", "v2", "auth"]
        [ UB.string "scope" Keys.getScopes
        , UB.string "access_type" "offline"
        , UB.string "state" "state_parameter_passthrough_value"
        , UB.string "redirect_uri" "http://localhost:8000/src/TestIframeResponse.elm"
        , UB.string "response_type" "code"
        , UB.string "client_id" Keys.getClientId
        ]
    --

-- UPDATE

type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | Connection

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
        Connection -> ({model | phase = Connecting}, Cmd.none)

-- SUB

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "identity endpoint test"
    , body =
        [ p [] [text ("status: "++"")]
        , case model.phase of
            Init -> button [onClick Connection] [text "Connexion"]
            Connecting ->
                --
                iframe [width 400, height 500, src createIframeUrl] []
                --
        ]
    }
