module JsonData exposing (..)

import Api exposing (FileId, decodeInfoFile)

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Decode exposing (Decoder, dict, field, int, list, string)
import Json.Encode as JE

-- TYPES

type alias ProjectBase =
    { desc : String
    , nextId : Int
    , tasks : Dict String ProjectTask
    , topLevel : List String
    }

type alias ProjectInfo =
    { fileId : FileId
    , name : String
    , indicators : ProjectIndicators
    }

type alias TracyInfos =
    { projects : List ProjectInfo
    , refreshToken : Maybe String
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
    , mode : ProjectTaskMode
    , tmpTitle : String
    , tmpDesc : String
    , tmpPreview : Bool
    , title : String
    , parentId : String
    , status : ProjectTaskStatus
    , desc : String
    , indicators : ProjectIndicators
    , subTasks : List String
    }

-- GENERATION

generateTask : Bool -> String -> String -> ProjectTask
generateTask init title parentId =
    ProjectTask
        (if init then True else False)
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

decodeHome : Decoder TracyInfos
decodeHome =
    JD.map2 TracyInfos
        (field "projects" (list decodeProjectInfo))
        (JD.maybe (field "refreshToken" string))

decodeProjectTask : Decoder ProjectTask
decodeProjectTask =
    let
        getProjectTaskStatus status =
            case status of
                "Planned" -> Planned
                "Wip" -> Wip
                _ -> Closed
    in
    JD.map6 (ProjectTask False ModeView "" "" False)
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

encodeHome : (List ProjectInfo, String) -> JE.Value
encodeHome (projects, refreshToken) =
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
        [ ("projects", JE.list encodeProjectInfo projects)
        , ("refreshToken", JE.string refreshToken)
        ]

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
