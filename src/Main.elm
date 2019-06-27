port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, decodeValue, errorToString, field, string)
import Json.Encode as JE
import Url

-- TODO : connexion aux drive APIs
-- TODO : listing des projets
-- TODO : page d'accueil d'un projet
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

type Status
    = Connecting
    | LogIn
    | Home
    | Project

type Question
    = AskInit
    | AskLogIn
    | AskLogOut

type Answer
    = Error String
    | Connected
    | Unconnected

type ShowErr
    = ErrYes String
    | ErrNo

type alias Model =
    { status : Status
    , prevStatus : Status
    , withErr : ShowErr
    }

init : () -> (Model, Cmd Msg)
init _ =
    (Model Connecting LogIn ErrNo, ask (prepareRequest AskInit))

-- JSON ENCODE

prepareRequest : Question -> JE.Value
prepareRequest question =
    JE.object
        [ ("question", case question of
            AskInit -> JE.string "Starting"
            AskLogIn -> JE.string "Authenticate"
            AskLogOut -> JE.string "SignOut"
        ), ("data", case question of
            _ -> JE.string ""
        )]

-- JSON DECODE

decodeAnswer : JE.Value -> Answer
decodeAnswer value =
    let
        checkStatus = decodeValue (field "status" string) value
    in
    case checkStatus of
        Err error -> Error ("checkStatus error: "++(errorToString error))
        Ok status ->
            case status of
                "Error" ->
                    let
                        checkDesc = decodeValue (field "desc" string) value
                    in
                    case checkDesc of
                        Err error -> Error ("checkDesc error: "++(errorToString error))
                        Ok desc -> Error desc
                "Connected" -> Connected
                "Unconnected" -> Unconnected
                _ -> Error "Something went wrong, bad value for status"

-- UPDATE

type Msg
    = Asking Question
    | ReceptionData JE.Value

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Asking question ->
            --
            -- TODO : comment faire la demande ?
            --
            (model, ask (JE.string "test"))
        ReceptionData value ->
            let
                answer = decodeAnswer value
            in
            case answer of
                Error info ->
                    ({model | status = model.prevStatus, withErr = ErrYes info}, Cmd.none)
                Unconnected ->
                    ({model | status = LogIn}, Cmd.none)
                Connected ->
                    ({model | status = Home}, Cmd.none)
                --
                -- TODO : missing answers
                --
                --_ -> (model, Cmd.none)
            --

-- PORTS

port ask : JE.Value -> Cmd msg
port received : (JE.Value -> msg) -> Sub msg

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
    received ReceptionData

-- VIEW

view : Model -> Browser.Document Msg
view model =
    let
        errorMsg =
            case model.withErr of
                ErrYes info -> div [class "error"] [text info]
                ErrNo -> div [class "hide"] []
    in
    { title = "Tracy"
    , body = [
        --h1 [] [text "Tracy"]
        div [class "core"] (errorMsg::[
            (case model.status of
                Connecting -> div [class "waiter"] [text "Connexion en cours ..."]
                LogIn -> viewLogIn
                Home -> viewHome
                Project -> div [] []
            )])
        ]
    }

viewLogIn : Html Msg
viewLogIn =
    --
    --
    div [] [text "It's time to log in"]
    --

viewHome : Html Msg
viewHome =
    --
    --
    div [] [text "Welcome Home!"]
    --
