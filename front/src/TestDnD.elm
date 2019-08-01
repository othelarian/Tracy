import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, preventDefaultOn, stopPropagationOn)
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
    , parentId : TaskId
    , subTasks : List TaskId
    }

type alias Model =
    { tasks : Dict TaskId Task
    , dragRun : Bool
    , tmpDragId : TaskId
    , tmpDragParent : TaskId
    , tmpDragOrder : List TaskId
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
        newModel = Model newTasks False "" "" [] ["0", "4", "5", "6"]
    in
    (newModel, Cmd.none)

-- HANDLERS

dragHandler :
    { task: Task                -- the actual dragged task
    , tasks: Dict TaskId Task   -- the good old dict with all the tasks
    , topLevel: List TaskId     -- the first level task
    , dragId: TaskId            -- the id of the actual dragged task
    , dragParent: TaskId        -- the id of the parent we want the task be linked to
    , dragOrder: List TaskId    -- the order of the subtasks we want to apply for the parent
    }
    -> (List TaskId, Dict TaskId Task)
dragHandler data =
    let
        (cancelTopLevel, cancelTasks) =
            let filtering theList = List.filter (\n -> n /= data.dragId) theList in
            
            if data.task.parentId == "-1" then (filtering data.topLevel, data.tasks)
            else
                let
                    doCancelUpdate maybeParent =
                        case maybeParent of
                            Just parentTask ->
                                Just {parentTask | subTasks = filtering parentTask.subTasks}
                            Nothing -> Nothing
                in
                (data.topLevel, Dict.update data.task.parentId doCancelUpdate data.tasks)
        (oldTopLevel, oldTasks) =
            if data.dragParent == "-1" then (data.dragOrder, cancelTasks)
            else
                let
                    doOldUpdate maybeParent =
                        case maybeParent of
                            Just parentTask -> Just {parentTask | subTasks = data.dragOrder}
                            Nothing -> Nothing
                in
                (cancelTopLevel, Dict.update data.dragParent doOldUpdate cancelTasks)
        doLastUpdate maybeTask =
            case maybeTask of
                Just theTask -> Just {theTask | parentId = data.dragParent}
                Nothing -> Nothing
        finalTasks = Dict.update data.dragId doLastUpdate oldTasks
    in
    (oldTopLevel, finalTasks)

-- UPDATE

type Msg
    = NoOp
    | ToggleTask TaskId
    | DragStart TaskId
    | DragMove TaskId TaskId
    | DragAdd TaskId
    | DragOver
    | DragCancel
    | DragDrop

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
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
                    let
                        tmpListTasks =
                            if task.parentId == "-1" then model.topLevel
                            else
                                case Dict.get task.parentId model.tasks of
                                    Just parentTask -> parentTask.subTasks
                                    Nothing -> []
                    in
                    ({model
                        | dragRun = True
                        , tmpDragId = taskId
                        , tmpDragParent = task.parentId
                        , tmpDragOrder = tmpListTasks
                        }
                    , Cmd.none)
                Nothing -> (model, Cmd.none)
        DragMove parentId taskId ->
            --
            -- TODO : récupération de la tâche, pour accéder à son parent
            -- TODO : récupération des subtasks du parent / du topLevel
            -- TODO : suivant la posi
            --
            let
                --
                --
                index theList = List.indexedMap Tuple.pair theList
                --
            in
            if parentId == "-1" then
                --
                index model.topLevel
                --
                --
            else
                let
                    --
                    --
                    case Dict.get parentId model.tasks of
                    --
                in
            --
            --
            (model, Cmd.none)
            --
        DragAdd taskId ->
            case Dict.get taskId model.tasks of
                Just task ->
                    let
                        (addTopLevel, addTasks) = dragHandler
                            { task = task
                            , tasks = model.tasks
                            , topLevel = model.topLevel
                            , dragId = model.tmpDragId
                            , dragParent = taskId
                            , dragOrder = [model.tmpDragId]
                            }
                    in
                    ({model | topLevel = addTopLevel, tasks = addTasks}, Cmd.none)
                Nothing -> (model, Cmd.none)
        DragOver -> (model, Cmd.none)
        DragCancel ->
            case model.dragRun of
                True ->
                    let
                        prepareModel = {model | dragRun = False}
                        finalModel modModel =
                            {modModel
                                | tmpDragId = ""
                                , tmpDragParent = ""
                                , tmpDragOrder = []
                                }
                    in
                    case Dict.get model.tmpDragId model.tasks of
                        Just task ->
                            let
                                (cancelTopLevel, cancelTasks) = dragHandler
                                    { task = task
                                    , tasks = model.tasks
                                    , topLevel = model.topLevel
                                    , dragId = model.tmpDragId
                                    , dragParent = model.tmpDragParent
                                    , dragOrder = model.tmpDragOrder
                                    }
                                updatedModel = {prepareModel
                                    | tasks = cancelTasks
                                    , topLevel = cancelTopLevel
                                    }
                            in
                            (finalModel updatedModel, Cmd.none)
                        Nothing -> (finalModel prepareModel, Cmd.none)
                False -> (model, Cmd.none)
        DragDrop ->
            ( {model | tmpDragId = "", tmpDragParent = "", tmpDragOrder = [], dragRun = False}
            , Cmd.none)

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "test drag'n'drop"
    , body =
        [ div []
            (viewTasks model.tasks model.tmpDragId model.topLevel)
        ]
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
                div
                    [ attribute "style" "border: dotted 1px gray;width:200px;"
                    , dropzone "true"
                    , on "dragend" (Decode.succeed DragCancel)
                    , preventDefaultOn "dragover" (Decode.map prevDef (Decode.succeed DragOver))
                    , preventDefaultOn "drop" (Decode.map prevDef (Decode.succeed DragDrop))
                    ]
                    [div [] [text "&amp;"]]
            else
                let
                    taskTitle =
                        div
                            [ onClick (ToggleTask taskId)
                            , draggable (if task.opened then "false" else "true")
                            , on "dragstart" (Decode.succeed (DragStart taskId))
                            , on "dragend" (Decode.succeed DragCancel)
                            , style "background" "yellow"
                            ]
                            [text task.title]
                in
                div
                    (List.append
                        (if task.opened then []
                        else [stopPropagationOn
                            "dragenter"
                            (Decode.map prevDef (Decode.succeed (DragMove task.parentId taskId)))])
                        [attribute "style" "border:solid 1px black;width:200px;"]
                    )
                    (taskTitle::(if task.opened then
                        [div [style "margin" "3px"] 
                            (if List.isEmpty task.subTasks then
                                [div
                                    [ stopPropagationOn
                                        "dragenter"
                                        (Decode.map prevDef (Decode.succeed (DragAdd taskId)))
                                    , attribute "style" "height:10px;background:blue;"
                                    ]
                                    []
                                ]
                            else
                                (viewTasks tasks dragId task.subTasks)
                            )
                        ]
                    else []))
        Nothing -> text ""
