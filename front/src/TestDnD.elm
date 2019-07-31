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

-- UPDATE

type Msg
    = NoOp
    | ToggleTask TaskId
    | DragStart TaskId
    --| DragEnter
    --| DragLeave
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
            --
        --DragEnter -> ({model | dragOver = True}, Cmd.none)
        --DragLeave -> ({model | dragOver = False}, Cmd.none)
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
                            --
                            --
                            -- TODO : mise à jour de la tâche parente actuelle -> OK
                            -- TODO : mise à jour de l'ancienne tâche parente
                            -- TODO : mise à jour de la tâche
                            --
                            --
                            let
                                (cancelTopLevel, cancelParentTasks) =
                                    if task.parentId == "-1" then
                                        ( List.filter
                                            (\n -> n /= model.tmpDragId)
                                            prepareModel.topLevel
                                        , model.tasks)
                                    else
                                        let
                                            doCancelParent maybeTask =
                                                case maybeTask of
                                                    Just parentTask ->
                                                        let
                                                            filteredSubTasks =
                                                                List.filter
                                                                    (\n -> n /= model.tmpDragId)
                                                                    parentTask.subTasks
                                                        in
                                                        Just {parentTask | subTasks = filteredSubTasks}
                                                    Nothing -> Nothing
                                        in
                                        ( prepareModel.topLevel
                                        , Dict.update task.parentId doCancelParent prepareModel.tasks)
                                (oldTopLevel, oldParentTasks) =
                                    if model.tmpDragParent == "-1" then
                                        (model.tmpDragOrder, cancelParentTasks)
                                    else
                                        let
                                            --
                                            doUpdateParent maybeTask =
                                                case maybeTask of
                                                    Just parentTask ->
                                                        --
                                                        --
                                                        --
                                                    Nothing -> Nothing
                                            --
                                            --
                                        in
                                        --
                                        ( cancelTopLevel
                                        , ())
                                        --
                                --
                                --
                                --updatedModel = {prepareModel | tasks = ???}
                                updatedModel = prepareModel
                                --
                            in
                            (finalModel updatedModel, Cmd.none)
                        Nothing -> (finalModel prepareModel, Cmd.none)
                False -> (model, Cmd.none)
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
    , body =
        [ div
            []
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
                --
                --
                div
                    [ attribute "style" "border: dotted 1px gray;width:200px;"
                    , dropzone "true"
                    , on "dragend" (Decode.succeed DragCancel)
                    , preventDefaultOn "dragover" (Decode.map prevDef (Decode.succeed DragOver))
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
                            , on "dragend" (Decode.succeed DragCancel)
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
