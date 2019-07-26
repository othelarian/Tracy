import Keys

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, iframe, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Url
import Url.Builder as UB
import Url.Parser as UP
import Url.Parser.Query as UPQ

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

type Phase
    = Init

type alias Model =
    { url : Url.Url
    , phase : Phase
    }

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url _ =
    let
        --
        _ = Debug.log "TODO" "parse query"
        --
        getCode = Nothing --UP.parse parseQueryCode url
        --
    in
    ( Model url Init
    , case getCode of
        Just code -> Cmd.none
        Nothing -> Cmd.none
    )

-- HANDLERS

createIdentityUrl : String
createIdentityUrl =
    UB.crossOrigin
        "https://accounts.google.com"
        ["o", "oauth2", "v2", "auth"]
        [ UB.string "scope" Keys.getScopes
        , UB.string "access_type" "offline"
        , UB.string "state" "state_parameter_passthrough_value"
        , UB.string "redirect_uri" "http://localhost:8000/test.html"
        , UB.string "response_type" "code"
        , UB.string "client_id" Keys.getClientId
        ]

parseQueryCode : UPQ.Parser (Maybe String)
parseQueryCode =
    UPQ.string "code"

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
        UrlRequested urlRequested -> (model, Cmd.none)
        Connection -> (model, Nav.load createIdentityUrl)

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
        , p [] [text ("url: "++(Url.toString model.url))]
        , case model.phase of
            Init -> button [onClick Connection] [text "Connexion"]
        ]
    }
