module JsonData exposing (..)

import Api exposing (FileId, decodeInfoFile)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Decode exposing (Decoder, array, dict, field, int, list, string)
import Json.Encode as JE

-- TYPES

type alias ProjectBase =
    { desc : String
    , nextId : Int
    , tasks : Dict String ProjectTask
    , topLevel : Array String
    }

type alias ProjectInfo =
    { fileId : FileId
    , name : String
    , values : Array Int
    }

type ProjectTaskStatus
    = Planned
    | Wip
    | Closed

type alias ProjectTask =
    { opened : Bool
    , editing : Bool
    , tmpTitle : String
    , tmpDesc : String
    , title : String
    , parentId : String
    , status : ProjectTaskStatus
    , desc : String
    , subTasks : Array String
    }

-- GENERATION

generateTask : Bool -> String -> String -> ProjectTask
generateTask init title parentId =
    ProjectTask
        (if init then True else False)
        (if init then True else False)
        title
        ""
        title
        parentId
        Planned
        ""
        Array.empty

-- JSON DECODE

decodeProjectInfo : Decoder ProjectInfo
decodeProjectInfo =
    JD.map3 ProjectInfo
        (field "fileId" string)
        (field "name" string)
        (field "values" (array int))

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
    JD.map5 (ProjectTask False False "" "")
        (field "title" string)
        (field "parentId" string)
        (field "status" (JD.map getProjectTaskStatus string))
        (field "desc" string)
        (field "subTasks" (array string))

decodeProject : Decoder ProjectBase
decodeProject =
    JD.map4 ProjectBase
        (field "desc" JD.string)
        (field "nextId" int)
        (field "tasks" (dict decodeProjectTask))
        (field "topLevel" (array string))

-- JSON ENCODE

encodeHome : List ProjectInfo -> JE.Value
encodeHome projects =
    let
        encodeProjectInfo : ProjectInfo -> JE.Value
        encodeProjectInfo projectInfo =
            JE.object
                [ ("fileId", JE.string projectInfo.fileId)
                , ("name", JE.string projectInfo.name)
                , ("values", JE.array JE.int projectInfo.values)
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
        , ("parentId", JE.string task.parentId)
        , ("status", JE.string (encodeProjectTaskStatus task.status))
        , ("desc", JE.string task.desc)
        , ("subTasks", JE.array JE.string task.subTasks)
        ]

encodeProject : ProjectBase -> JE.Value
encodeProject base =
    JE.object
        [ ("desc", JE.string base.desc)
        , ("nextId", JE.int base.nextId)
        , ("tasks", JE.dict (\n -> n) encodeProjectTask base.tasks)
        , ("topLevel", JE.array JE.string base.topLevel)
        ]
