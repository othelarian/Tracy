import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, preventDefaultOn)
import Json.Decode as Decode

import Debug

-- MAIN

main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- MODEL

type alias TaskId = String

type alias Task =
    { opened : Bool
    , title : String
    , parentId : String
    , subTasks : List TaskId
    }

type alias Model =
    { tasks : Dict TaskId Task
    , dragOver : Bool
    , tmpDragId : TaskId
    , tmpDragParent : TaskId
    , topLevel : List TaskId
    }

init : () -> (Model, Cmd Msg)
init _ =
    let
        newTasks = Dict.empty
            |> Dict.insert "0" (Task False "tache 1" "-1" ["1", "3"])
            |> Dict.insert "1" (Task False "tache 1.2" "0" ["2"])
            |> Dict.insert "2" (Task False "tache 1.2.1" "1" [])
            |> Dict.insert "3" (Task False "tache 1.3" "0" [])
            |> Dict.insert "4" (Task False "tache 2" "-1" [])
            |> Dict.insert "5" (Task False "tache 3" "-1" [])
            |> Dict.insert "6" (Task False "tache 4" "-1" ["7", "8", "9"])
            |> Dict.insert "7" (Task False "tache 4.1" "6" [])
            |> Dict.insert "8" (Task False "tache 4.2" "6" [])
            |> Dict.insert "9" (Task False "tache 4.3" "6" [])
        newModel = Model newTasks False "" "" ["0", "4", "5", "6"]
    in
    (newModel, Cmd.none)

-- UPDATE

type Msg
    = ToggleTask TaskId
    | DragStart TaskId
    --| DragEnter
    --| DragLeave
    --| DragOver
    | DragCancel
    | DragDrop

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ToggleTask taskId ->
            let
                doOpen maybeTask =
                    case maybeTask of
                        Just task -> Just {task | opened = not task.opened}
                        Nothing -> Nothing
                updatedTasks = Dict.update taskId doOpen model.tasks
                newModel = {model | tasks = updatedTasks}
            in
            (newModel, Cmd.none)
        DragStart taskId ->
            case Dict.get taskId model.tasks of
                Just task ->
                    {-
                    let dragModel = {model | dragRun = True, tmpDrag = taskId} in
                    if task.parentId == "-1" then
                        ({dragModel | topLevel = (List.filter (\n -> n /= taskId) model.topLevel)}
                        , Cmd.none)
                    else
                        let
                            doStartDrag maybeParentTask =
                                case maybeParentTask of
                                    Just parentTask ->
                                        Just {parentTask
                                            | subTasks = List.filter (\n -> n /= taskId) parentTask.subTasks}
                                    Nothing -> Nothing
                            draggedTasks = Dict.update task.parentId doStartDrag model.tasks
                        in
                        ({dragModel | tasks = draggedTasks}, Cmd.none)
                    -}
                    --
                    ({model | dragOver = True, tmpDragId = taskId, tmpDragParent = task.parentId}, Cmd.none)
                    --
                Nothing -> (model, Cmd.none)
            --
        --DragEnter -> ({model | dragOver = True}, Cmd.none)
        --DragLeave -> ({model | dragOver = False}, Cmd.none)
        --DragOver ->
            --
            --
            --
            --
            --(model, Cmd.none)
        DragCancel ->
            --
            let
                _ = Debug.log "Drag" "CANCEL"
            in
            --
            (model, Cmd.none)
            --
        DragDrop ->
            --
            let
                --
                _ = Debug.log "Drag" "END"
                --
                --
            in
            --
            (model, Cmd.none)
            --

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "test drag'n'drop"
    , body = [div [] (viewTasks model.tasks model.tmpDragId model.topLevel)]
    }

viewTasks : Dict TaskId Task -> TaskId -> List TaskId -> List (Html Msg)
viewTasks tasks dragId taskIds =
    List.map (viewTask tasks dragId) taskIds

prevDef : msg -> (msg, Bool)
prevDef msg = (msg, True)

viewTask : Dict TaskId Task -> TaskId -> TaskId -> Html Msg
viewTask tasks dragId taskId =
    case Dict.get taskId tasks of
        Just task ->
            if taskId == dragId then
                --
                --
                div
                    [ attribute "style" "border: dotted 1px gray;width:200px;"
                    , dropzone "true"
                    --, preventDefaultOn "dragover" (Decode.map prevDef (Decode.succeed DragOver))
                    , preventDefaultOn "drop" (Decode.map prevDef (Decode.succeed DragDrop))
                    ]
                    [div [] [text "&amp;"]]
                --
            else
                let
                    taskTitle =
                        div
                            [ onClick (ToggleTask taskId)
                            , draggable "true"
                            , on "dragstart" (Decode.succeed (DragStart taskId))
                            , preventDefaultOn "dragend" (Decode.map prevDef (Decode.succeed DragCancel))
                            , style "background" "yellow"
                            ]
                            [text task.title]
                    --
                    --
                in
                div
                    [attribute "style" "border:solid 1px black;width:200px;"]
                    (taskTitle::(if task.opened then
                        --
                        [div [style "margin" "3px"] 
                            (if List.isEmpty task.subTasks then
                                []
                            else
                                (viewTasks tasks dragId task.subTasks)
                            )
                        ]
                        --
                        --
                    else []))
        Nothing -> text ""
