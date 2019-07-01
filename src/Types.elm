module Types exposing (
    Answer, ApiKey, FileId, HomePhase, Model, Msg, ProjectPhase, Question, ShowErr, Status, Token
    , init, modelChangeStatus, modelGotError)

import Json.Decode as JD
import Json.Encode as JE

type HomePhase
    = HomeCheck
    | HomeError
    | HomeInit
    | HomeGetInfo
    | HomeList
    --
    -- TODO : finir les phases
    --
    --
    | HomeClean
    --
    --| HomeAdd
    --| HomeRemove
    --

type ProjectPhase
    = ProjectBase
    | ProjectGraphics
    --|

type Status
    = Connecting
    | LogIn
    | Home HomePhase
    | Project ProjectPhase

type Question
    = AskInit
    | AskLogIn
    | AskLogOut

type Answer
    = AError String
    | Connected (Result JD.Error Token)
    | Unconnected

type ShowErr
    = ErrYes String
    | ErrNo


type alias FileId = String

type alias NeedClean = Bool

type alias ProjectId = String

type alias ProjectInfo =
    { fileId : FileId
    , name : String
    }

type alias ProjectData =
    { info : ProjectInfo
    --
    -- TODO : ajout des champs propre au projet
    --
    }

projectEmpty : ProjectData
projectEmpty =
    ProjectData (ProjectInfo "" "")

type alias Model =
    { status : Status
    , prevStatus : Status
    , api_key : ApiKey
    , token : Token
    , withErr : ShowErr
    , homeId : FileId
    , projects : List ProjectInfo
    , selected : ProjectData
    --
    -- TODO : testValue à supprimer, un jour
    --
    , testValue : String
    --
    }

modelChangeStatus : Model -> Status -> Status -> Model
modelChangeStatus model actual previous =
    {model | status = actual, prevStatus = previous, withErr = ErrNo }

modelGotError : Model -> String -> Model
modelGotError model error =
    let
        newModel = {model | withErr = ErrYes error}
    in
    case model.status of
        Connecting -> {newModel | status = LogIn}
        LogIn -> model -- cas impossible
        Home phase ->
            case phase of
                HomeCheck ->
                    {newModel | status = Home HomeError, prevStatus = Home HomeCheck}
                HomeError ->
                    {newModel | prevStatus = Home HomeError}
                HomeInit ->
                    {newModel | status = Home HomeError, prevStatus = Home HomeCheck}
                HomeGetInfo ->
                    {newModel | status = model.prevStatus, prevStatus = model.status}
                --
                --
                -- TODO : faire les autres cas
                --
                _ -> newModel
                --
        Project phase ->
            --
            -- TODO : finaliser cette partie
            --
            model
            --

init : ApiKey -> (Model, Cmd Msg)
init apiKey =
    (Model Connecting LogIn apiKey "" ErrNo "" [] projectEmpty "", ask "Starting")

type Msg
    = Asking Question
    | ReceptionData JE.Value
    | GetListFiles (Result Http.Error (List InfoFile))
    | GetCreateFile (Result Http.Error String)
    | GetReadHome (Result Http.Error (List InfoFile))
    --
    --
    | GetReadProject (Result Http.Error String)
    --
    | AddProject
    --
    | CancelProject
    --
    | CreateProject
    --
    --| GetUpdateFile
    --| GetDeleteFile
    --
    | HomeRetry
    --
    -- TODO : la liste des messages continue
    --
    | TestList
    | TestCreate
    | TestRead
    | TestDelete
    | TestRep (Result Http.Error (List String))
    | TestChange String
    --
