import Browser
import Browser.Events as BE
import Dict exposing (Dict)
import DnDList.Groups
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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

type alias Group = TaskId

type alias Task =
    { opened : Bool
    , title : String
    , parentId : TaskId
    , subTasks : List TaskId
    }

type alias DnDTask =
    { id : TaskId
    , group : Group
    , foot : Bool
    }

type alias Model =
    { tasks : Dict TaskId Task
    , dnd : DnDList.Groups.Model
    , dndItems : List DnDTask
    , dragRunning : Maybe (Int, DnDTask)
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
        topLevel = ["0", "4", "5", "6"]
        dndItems =
            List.foldr (\n a -> (createItem n "-1")::a) [(DnDTask "f-1" "-1" True)] topLevel
    in
    (Model newTasks system.model dndItems Nothing topLevel, Cmd.none)

-- DND SYSTEM

config: DnDList.Groups.Config DnDTask
config =
    { beforeUpdate = \_ _ list -> list
    , listen = DnDList.Groups.OnDrag
    , operation = DnDList.Groups.Rotate
    , groups =
        { listen = DnDList.Groups.OnDrag
        , operation = DnDList.Groups.InsertBefore
        , comparator = comparator
        , setter = setter
        }
    }

comparator : DnDTask -> DnDTask -> Bool
comparator task1 task2 = task1.group == task2.group

setter : DnDTask -> DnDTask -> DnDTask
setter task1 task2 = {task2 | group = task1.group}

system : DnDList.Groups.System DnDTask Msg
system = DnDList.Groups.create config DnDMsg

-- HANDLERS

calculateOffset : Int -> Group -> List DnDTask -> Int
calculateOffset index group list =
    case list of
        [] -> 0
        x :: xs ->
            if x.group == group then index
            else calculateOffset (index + 1) group xs

createItem : TaskId -> Group -> DnDTask
createItem taskId group = DnDTask taskId group False

handleDnDUpdate : Int -> DnDTask -> List DnDTask -> Dict String Task -> (List TaskId, Dict String Task)
handleDnDUpdate dragIndex item dndItems oldTasks =
    let
        filteredItems g i a = if i.group == g then i.id::a else a
        genTopLevel = List.foldr (filteredItems "-1") [] dndItems
        genTasks =
            case Dict.get item.id oldTasks of
                Just task ->
                    let
                        doFirstUpdate maybeTask =
                            case maybeTask of
                                Just tmpTask -> Just {tmpTask | parentId = item.group}
                                Nothing -> Nothing
                        firstUpdate = Dict.update item.id doFirstUpdate oldTasks
                        doSecondUpdate maybeTask =
                            case maybeTask of
                                Just tmpTask ->
                                    let
                                        newSubTasks =
                                            if task.parentId == item.group then
                                                List.foldr (filteredItems item.group) [] dndItems
                                            else
                                                List.filter (\n -> n /= item.id) tmpTask.subTasks
                                    in
                                    Just {tmpTask | subTasks = newSubTasks}
                                Nothing -> Nothing
                        secondUpdate =
                            if task.parentId /= "-1" then
                                Dict.update task.parentId doSecondUpdate firstUpdate
                            else firstUpdate
                        doThirdUpdate maybeTask =
                            case maybeTask of
                                Just tmpTask ->
                                    let newSubTasks = List.filter (\n -> n /= item.id) tmpTask.subTasks in
                                    Just {tmpTask | subTasks = newSubTasks}
                                Nothing -> Nothing
                        thirdUpdate =
                            if item.group /= "-1" && (item.group /= task.parentId) then
                                Dict.update item.group doThirdUpdate secondUpdate
                            else secondUpdate
                    in
                    thirdUpdate
                Nothing -> oldTasks
    in
    (genTopLevel, genTasks)

-- UPDATE

type Msg
    = NoOp
    | ToggleTask TaskId
    | DnDMsg DnDList.Groups.Msg
    | CheckTopLevel

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
                updatedDnDItems =
                    case Dict.get taskId updatedTasks of
                        Just task ->
                            if task.opened then
                                List.map (\n -> createItem n taskId) task.subTasks
                                    |> (\a b -> List.append b a) [DnDTask ("f"++taskId) taskId True]
                                    |> List.append model.dndItems
                            else
                                List.filter (\n -> n.group /= taskId) model.dndItems
                        Nothing -> model.dndItems
                newModel = {model | tasks = updatedTasks, dndItems = updatedDnDItems}
            in
            (newModel, Cmd.none)
        DnDMsg dndMsg ->
            let
                (dnd, dndItems) = system.update dndMsg model.dnd model.dndItems
                (dragRunning, newTopLevel, newTasks) =
                    case system.info dnd of
                        Just {dragIndex} ->
                            case List.drop dragIndex dndItems |> List.head of
                                Just item ->
                                    let
                                        filteredFooters = List.filter (\n -> not n.foot) dndItems
                                        (genTopLevel, genTasks) =
                                            handleDnDUpdate dragIndex item filteredFooters model.tasks
                                    in
                                    (Just (dragIndex, item), genTopLevel, genTasks)
                                Nothing -> (Nothing, model.topLevel, model.tasks)
                        Nothing -> (Nothing, model.topLevel, model.tasks)
            in
            ( {model
                | dnd = dnd
                , dndItems = dndItems
                , dragRunning = dragRunning
                , tasks = newTasks
                , topLevel = newTopLevel
                }
            , system.commands model.dnd)
        CheckTopLevel ->
            let
                _ = Debug.log "TOP LEVEL" (Debug.toString model.topLevel)
                _ = Debug.log "DICT" (Debug.toString model.tasks)
            in
            (model, Cmd.none)

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
    system.subscriptions model.dnd

-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "test drag'n'drop"
    , body =
        [ section [] (viewTasks model model.dndItems "-1")
        , button [onClick CheckTopLevel] [text "CHECK"]
        , ghostTask model.dragRunning model.dnd
        ]
    }

viewTasks : Model -> List DnDTask -> TaskId -> List (Html Msg)
viewTasks model dndItems group =
    let
        listToShow = List.filter (\n -> n.group == group) dndItems
        offset = calculateOffset 0 group dndItems
    in
    (List.indexedMap (viewTask model dndItems offset) listToShow |> List.concat)

viewTask : Model -> List DnDTask -> Int -> Int -> DnDTask -> List (Html Msg)
viewTask model dndItems offset index item =
    let
        gIndex = offset + index
        itemId = "id-"++(String.fromInt gIndex)
        (dragIn, dragItem) =
            case model.dragRunning of
                Just (_, getItem) -> (True, getItem)
                Nothing -> (False, DnDTask "-1" "-2" False)
    in
    if item.foot then
        [div
            ([ id itemId
            , attribute "style" "height:10px;width:202px;background:blue;"
            ]++(if dragIn && item.group /= dragItem.group then system.dropEvents gIndex itemId else []))
            []]
    else
        if dragIn && item.id == dragItem.id then
            [div
                [ id itemId
                , style "background" "gray"
                , style "height" "20px"
                , style "width" "202px"
                ]
                []]
        else
            case Dict.get item.id model.tasks of
                Just task ->
                    (if dragIn && task.opened then
                        div
                            ([id itemId, style "height" "2px", style "width" "202px", style "background" "orange"]
                            ++system.dropEvents gIndex itemId)
                            []
                    else text "")::[div
                        (List.append
                            [ style "width" "200px"
                            , style "border" "solid 1px black"
                            , style "margin-top" (if not dragIn then "2px" else "0")
                            ]
                            (if dragIn && not task.opened then id itemId::system.dropEvents gIndex itemId
                            else
                                if not task.opened then id itemId::system.dragEvents gIndex itemId
                                else [id itemId]
                            )
                        )
                        (List.append
                            [div
                                [ onDoubleClick (ToggleTask item.id)
                                , style "background" "yellow"
                                , style "height" "20px"
                                ]
                                [text task.title]
                            ]
                            (if task.opened then
                                [div [style "margin" "10px"] (viewTasks model dndItems item.id)]
                            else [])
                        )]
                Nothing -> [text ""]

ghostTask : Maybe (Int, DnDTask) -> DnDList.Groups.Model -> Html Msg
ghostTask maybe dnd =
    case maybe of
        Just _ -> div
            ( [attribute "style" "background:green;width:202px;height:20px;"]
            ++ (system.ghostStyles dnd))
            []
        Nothing -> text ""
