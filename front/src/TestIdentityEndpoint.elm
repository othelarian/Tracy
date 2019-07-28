import Keys

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, iframe, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Url
import Url.Builder as UB
import Url.Parser as UP
import Url.Parser exposing(s, (<?>), (</>))
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

type Route
    = Sill
    | Lost
    | Gate String
    | Castle String

type Phase
    = Init

type alias Model =
    { url : Url.Url
    , route : Route
    , code : String
    , phase : Phase
    }

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url _ =
    let
        getRouteParser search = UP.query (UPQ.string search)
        getClearedUrl = {url | path = ""}
        getRefresh =
            case UP.parse (getRouteParser "refresh") getClearedUrl of
                Just res -> res
                Nothing -> Nothing
        getCode =
            case UP.parse (getRouteParser "code") getClearedUrl of
                Just res -> res
                Nothing -> Nothing
        getLost =
            case UP.parse (getRouteParser "lost") getClearedUrl of
                Just res -> res
                Nothing -> Nothing
        getRoute =
            case getRefresh of
                Just token -> Castle token
                Nothing -> case getCode of
                    Just code -> Gate code
                    Nothing -> case getLost of
                        Just _ -> Lost
                        Nothing -> Sill
    in
    ( Model url getRoute "" Init
    , case getRefresh of
        Just token ->
            --
            -- TODO : initialisation de l'app avec le refresh token
            --
            Cmd.none
            --
        Nothing ->
            case getCode of
                Just code ->
                    --
                    -- TODO : request the refresh token
                    --
                    -- TODO : pour les tests, on va d'abord envoyer un code bidon vers le rust pour voir si ça fonctionne
                    --
                    Http.get
                        { url = "/saveToken?token=abcde"
                        , expect = Http.expectString SaveAnswer
                        }
                    --
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

parseQueryCode : UP.Parser (Maybe String -> Maybe String) (Maybe String)
parseQueryCode =
    UP.query (UPQ.string "code")

-- UPDATE

type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | Connection
    | SaveAnswer (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url -> (model, Cmd.none)
        UrlRequested urlRequested -> (model, Cmd.none)
        Connection -> (model, Nav.load createIdentityUrl)
        SaveAnswer answer ->
            case answer of
                Ok rep ->
                    let
                        _ = Debug.log "rep" rep
                    in
                    --
                    --
                    (model, Cmd.none)
                    --
                Err _ -> (model, Cmd.none)

-- SUB

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "identity endpoint test"
    , body =
        [ p [] [text ("code: "++model.code)]
        , (
            case model.route of
                Sill -> p [] [text "sending SILL"]
                Lost -> p [] [text "We are lost..."]
                Gate _ -> p [] [text "Front of the gate, ready to use the code"]
                Castle _ -> p [] [text "This castle is mine!"]
        )
        , p [] [text ("url: "++(Url.toString model.url))]
        , case model.phase of
            Init -> button [onClick Connection] [text "Connexion"]
        ]
    }
