module Project exposing(Model, Msg(..), ProjectInfo, encodeProject, init, update, view)

import Api exposing (ApiCredentials, FileId)

import Html exposing (Html, button, div, p, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Json.Decode exposing (Decoder, field, int, list, string)
import Json.Encode as JE

-- MODEL

type alias ProjectInfo =
    { name : String
    , fileId : FileId
    }

type ProjectPhase
    = Loading
    | Viewing
    | Editing

type ProjectTaskStatus
    = Planned
    | Wip
    | Closed

type alias ProjectTask =
    { title : String
    , id : Int
    , parent : Int
    , status : ProjectTaskStatus
    , desc : String
    }

type alias ProjectBase =
    { desc : String
    , lastId : Int
    , tasks : List ProjectTask
    }

type alias Model =
    { apiCredentials : ApiCredentials
    , projectInfo : ProjectInfo
    , phase : ProjectPhase
    , base : ProjectBase
    , testValue : String
    , testShow : Bool
    }

init : ApiCredentials -> Bool -> Model
init apiCredentials testShow =
    Model apiCredentials (ProjectInfo "" "") Loading (ProjectBase "" 0 []) "" testShow

-- HANDLERS

createNewProject : String -> ProjectBase
createNewProject desc =
    ProjectBase desc 0 []

-- JSON DECODE

decodeProjectTask : ???
--
--
-- TODO : finir les decoders
--
--

decodeReadProject : Decoder ProjectBase
decodeReadProject =
    --
    -- TODO : décodage du json pour un projet
    --
    JD.map3 ProjectBase
        (field "desc" JD.string)
        (field "lastId" int)
        (field "tasks" (list (JD.map)))
    --

-- JSON ENCODE

encodeProjectTaskStatus : ProjectTaskStatus -> Int
encodeProjectTaskStatus status =
    case status of
        Planned -> 0
        Wip -> 1
        Closed -> 2

encodeProjectTask : ProjectTask -> JE.Value
encodeProjectTask task =
    JE.object
        [ ("title", JE.string task.title)
        , ("id", JE.int task.id)
        , ("parent", JE.int task.parent)
        , ("status", JE.int (encodeProjectTaskStatus task.status))
        , ("desc", JE.string task.desc)
        ]

encodeProject : ProjectBase -> JE.Value
encodeProject base =
    JE.object
        [ ("desc", JE.string base.desc)
        , ("lastId", JE.int base.lastId)
        , ("tasks", JE.list encodeProjectTask base.tasks)
        ]

-- UPDATE

type Msg
    = LoadProject FileId
    --
    --
    --
    | GoToHome ApiCredentials

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadProject fileId ->
            --
            --
            (model, Cmd.none)
            --
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
                Loading -> [span [] [text "Chargement en cours ..."]]
                --
                --
                --
                --
                _ -> [p [] [text "(en cours de construction)"]]
                --
            )
        , (case model.phase of
            --
            --
            --
            --
            _ -> div [class "waiter"] [text "Veuillez patienter"]
        )
        ]
