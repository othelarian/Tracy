module JsonData exposing (..)

import Api exposing (FileId, decodeInfoFile)

import Json.Decode as JD
import Json.Decode exposing (Decoder, field, int, list, string)
import Json.Encode as JE

-- TYPES

type alias ProjectBase =
    { desc : String
    , lastId : Int
    , tasks : List ProjectTask
    }

type alias ProjectInfo =
    { fileId : FileId
    , name : String
    , values : List Int
    }

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

-- JSON DECODE

decodeProjectInfo : Decoder ProjectInfo
decodeProjectInfo =
    JD.map3 ProjectInfo
        (field "fileId" string)
        (field "name" string)
        (field "values" (list int))

decodeHome : JD.Decoder (List ProjectInfo)
decodeHome =
    JD.field "projects" (list decodeProjectInfo)

decodeProjectTask : Decoder ProjectTask
decodeProjectTask =
    let
        getProjectTaskStatus status =
            case status of
                "Planned" -> Planned
                "Wip" -> Wip
                _ -> Closed
    in
    JD.map5 ProjectTask
        (field "title" string)
        (field "id" int)
        (field "parent" int)
        (field "status" (JD.map getProjectTaskStatus string))
        (field "desc" string)

decodeProject : Decoder ProjectBase
decodeProject =
    JD.map3 ProjectBase
        (field "desc" JD.string)
        (field "lastId" int)
        (field "tasks" (list decodeProjectTask))

-- JSON ENCODE

encodeHome : List ProjectInfo -> JE.Value
encodeHome projects =
    let
        encodeProjectInfo : ProjectInfo -> JE.Value
        encodeProjectInfo projectInfo =
            JE.object
                [ ("id", JE.string projectInfo.fileId)
                , ("name", JE.string projectInfo.name)
                , ("values", JE.list JE.int projectInfo.values)
                ]
    in
    JE.object [ ("projects", JE.list encodeProjectInfo projects) ]

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
        , ("id", JE.int task.id)
        , ("parent", JE.int task.parent)
        , ("status", JE.string (encodeProjectTaskStatus task.status))
        , ("desc", JE.string task.desc)
        ]

encodeProject : ProjectBase -> JE.Value
encodeProject base =
    JE.object
        [ ("desc", JE.string base.desc)
        , ("lastId", JE.int base.lastId)
        , ("tasks", JE.list encodeProjectTask base.tasks)
        ]
