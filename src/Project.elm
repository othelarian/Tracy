module Project exposing(Model, Msg, ProjectInfo, encodeNewProject, init, update, view)

import Api exposing (ApiCredentials, FileId)

import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Json.Decode as JD
import Json.Decode exposing (Decoder, field, string)
import Json.Encode as JE

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

-- JSON ENCODE

encodeNewProject : String -> String -> JE.Value
encodeNewProject name desc =
    JE.object
        [ ("name", JE.string name)
        , ("desc", JE.string desc)
        --
        -- TODO : ajouter les champs manquants
        --
        ]

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
    div [class "core"]
        [ div [class "zone_status"]
            --
            -- TODO : transformer la list ci-dessous en un case pour la zone de statut
            --
            []
            --
        , --
            --
            -- TODO : ici se passe la construction de la page
            --
            div [] ["(page de gestion d'un projet"]
            --
