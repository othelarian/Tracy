module TracyTask exposing (..)

import JsonData exposing (ProjectTask, ProjectTaskStatus)

import Dict exposing (Dict)
import Html exposing (Html, div, input, p, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

-- MODEL

type SubTasks = SubTasks (Dict Int Model)

type alias Model =
    { opened : Bool
    , editing : Bool
    , title : String
    , id : Int
    , parent : Int
    , order : Int
    , status : ProjectTaskStatus
    , desc : String
    , subTasks : SubTasks
    }

generate : ProjectTask -> Model
generate projectTask =
    Model
        False
        False
        projectTask.title
        projectTask.id
        projectTask.parent
        projectTask.order
        projectTask.status
        projectTask.desc
        (SubTasks Dict.empty)

populate : List ProjectTask -> Int -> Dict Int Model
populate projectTasks parentId =
    let
        subTasksList = List.filter (\n -> n.parent == parentId) projectTasks
    in
    List.map generate subTasksList
        |> List.map (\n -> {n | subTasks = SubTasks (populate projectTasks n.id)})
        |> List.map (\n -> (n.id, n))
        |> Dict.fromList

--init : Bool -> Bool -> ProjectTask -> (Model, Cmd Msg)
--init open edit projectTask =
--    (Model open edit projectTask.title, Cmd.none)

-- UPDATE

type Msg
    = SwitchEdit
    | ChangeTitle String

update : Model -> Msg -> (Model, Cmd Msg)
update model msg =
    case msg of
        SwitchEdit -> ({model | editing = not model.editing}, Cmd.none)
        ChangeTitle newTitle -> ({model | title = newTitle}, Cmd.none)
        --
        --

-- VIEW

view : Model -> Html Msg
view model =
    --
    -- TODO : c'est dans cette partie qu'on va trouver tout ce qui touche à une tâche
    --
    div []
        [ p [class "button"]
            [ (
                if model.editing then
                    input [onInput ChangeTitle] []
                else
                    span [] [text model.title]
                )
            ]
        ]
    --
