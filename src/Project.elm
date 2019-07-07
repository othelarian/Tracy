module Project exposing(Model, Msg, ProjectInfo, init, update, view)

import Api exposing (ApiCredentials, FileId)

import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Json.Decode as JD
import Json.Decode exposing (Decoder, field, string)

-- MODEL

type alias ProjectInfo =
    { name : String
    , fileId : FileId
    }

type alias Model =
    { apiCredentials : ApiCredentials
    , projectInfo : ProjectInfo
    --
    --
    , testValue : String
    --
    }

init : ApiCredentials -> Model
init apiCredentials =
    Model apiCredentials (ProjectInfo "" "") ""

-- JSON DECODE

decodeReadProject : Decoder String
decodeReadProject =
    --
    -- TODO : décodage du json pour un projet
    --
    field "name" string
    --

-- UPDATE

type Msg
    = InitProject
    --
    --

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        --
        --
        --
        _ -> (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
    --
    -- TODO : tout reste à faire
    --
    div [] [text "page de projet"]
