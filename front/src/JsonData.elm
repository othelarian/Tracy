module JsonData exposing (..)

import Api exposing (FileId, decodeInfoFile)

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Decode exposing (Decoder, dict, field, int, list, string)
import Json.Encode as JE

-- TYPES

type alias TaskId = String

type alias ProjectBase =
    { desc : String
    , nextId : Int
    , tasks : Dict TaskId ProjectTask
    , topLevel : List TaskId
    }

type alias ProjectInfo =
    { fileId : FileId
    , name : String
    , indicators : ProjectIndicators
    }

type ProjectTaskStatus
    = Planned
    | Wip
    | Closed

type ProjectTaskMode
    = ModeView
    | ModeEdit
    | ModeRemove

type alias ProjectIndicators =
    { wait : Int
    , wip : Int
    , done : Int
    }

type alias ProjectTask =
    { opened : Bool
    , moved : Bool
    , mode : ProjectTaskMode
    , tmpTitle : String
    , tmpDesc : String
    , tmpPreview : Bool
    , title : String
    , parentId : TaskId
    , status : ProjectTaskStatus
    , desc : String
    , indicators : ProjectIndicators
    , subTasks : List TaskId
    }

-- GENERATION

generateTask : Bool -> String -> TaskId -> ProjectTask
generateTask init title parentId =
    ProjectTask
        (if init then True else False)
        False
        (if init then ModeEdit else ModeView)
        title
        ""
        False
        title
        parentId
        Planned
        ""
        (ProjectIndicators 0 0 0)
        []

-- JSON DECODE

decodeIndicators : Decoder ProjectIndicators
decodeIndicators =
    JD.map3 ProjectIndicators
        (field "wait" int)
        (field "wip" int)
        (field "done" int)

decodeProjectInfo : Decoder ProjectInfo
decodeProjectInfo =
    JD.map3 ProjectInfo
        (field "fileId" string)
        (field "name" string)
        (field "indicators" decodeIndicators)

decodeHome : Decoder (List ProjectInfo)
decodeHome =
    field "projects" (list decodeProjectInfo)

decodeProjectTask : Decoder ProjectTask
decodeProjectTask =
    let
        getProjectTaskStatus status =
            case status of
                "Planned" -> Planned
                "Wip" -> Wip
                _ -> Closed
    in
    JD.map6 (ProjectTask False False ModeView "" "" False)
        (field "title" string)
        (field "parentId" string)
        (field "status" (JD.map getProjectTaskStatus string))
        (field "desc" string)
        (field "indicators" decodeIndicators)
        (field "subTasks" (list string))

decodeProject : Decoder ProjectBase
decodeProject =
    JD.map4 ProjectBase
        (field "desc" JD.string)
        (field "nextId" int)
        (field "tasks" (dict decodeProjectTask))
        (field "topLevel" (list string))

-- JSON ENCODE

encodeIndicators : ProjectIndicators -> JE.Value
encodeIndicators indicators =
    JE.object
        [ ("wait", JE.int indicators.wait)
        , ("wip", JE.int indicators.wip)
        , ("done", JE.int indicators.done)
        ]

encodeHome : List ProjectInfo -> JE.Value
encodeHome projects =
    let
        encodeProjectInfo : ProjectInfo -> JE.Value
        encodeProjectInfo projectInfo =
            JE.object
                [ ("fileId", JE.string projectInfo.fileId)
                , ("name", JE.string projectInfo.name)
                , ("indicators", encodeIndicators projectInfo.indicators)
                ]
    in
    JE.object
        [("projects", JE.list encodeProjectInfo projects)]

encodeProjectTask : ProjectTask -> JE.Value
encodeProjectTask task =
    let
        encodeProjectTaskStatus status =
            case status of
                Planned -> "Planned"
                Wip -> "Wip"
                Closed -> "Closed"
    in
    JE.object
        [ ("title", JE.string task.title)
        , ("parentId", JE.string task.parentId)
        , ("status", JE.string (encodeProjectTaskStatus task.status))
        , ("desc", JE.string task.desc)
        , ("indicators", encodeIndicators task.indicators)
        , ("subTasks", JE.list JE.string task.subTasks)
        ]

encodeProject : ProjectBase -> JE.Value
encodeProject base =
    JE.object
        [ ("desc", JE.string base.desc)
        , ("nextId", JE.int base.nextId)
        , ("tasks", JE.dict (\n -> n) encodeProjectTask base.tasks)
        , ("topLevel", JE.list JE.string base.topLevel)
        ]
