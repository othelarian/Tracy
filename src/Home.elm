module Home exposing(Model, Msg(..), init, update, view)

import Api exposing (..)
import Graphics exposing (..)
import JsonData
import JsonData exposing (ProjectInfo)
import Project exposing (createNewProject, init)

import Dict
import Html exposing (Html, a, button, div, input, label, p, span, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Random
import Task

import Debug

-- MODEL

type alias HomeError =
    { phase : HomePhase
    , info : String
    }

type alias Adding = Bool

type alias Validation = Bool

type HomePhase
    = Checking
    | Failing HomeError
    | Initializing
    | Filling
    | Listing Adding
    | Creating String
    | Updating JE.Value 
    | Removing Validation ProjectInfo

type alias ListProjects = List ProjectInfo

type alias Model =
    { apiCredentials : ApiCredentials
    , homeId : FileId
    , projects : ListProjects
    , phase : HomePhase
    , newProjectName : String
    , newProjectDesc : String
    , testValue : String
    , testShow : Bool
    }

init : ApiCredentials -> Bool -> Model
init apiCredentials testShow =
    Model apiCredentials "" [] Checking "" "" "" testShow

-- HANDLERS

handleInit : Model -> (Model, Cmd Msg)
handleInit model =
    let
        jsonValue = JE.object [ ("projects", JE.list JE.string []) ]
    in
    ({model | phase = Initializing}
    , apiCreateFile (FSCreate "tracy" jsonValue) CreateInit model.apiCredentials)

handleNewNameProject : Model -> (Model, Cmd Msg)
handleNewNameProject model =
    let
        rngList = List.map String.fromChar (String.toList "bcdefghijklmnopqrstuvwxyz0123456789-_ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        randomNameGenerator = Random.list 20 (Random.uniform "a" rngList)
    in
    (model, Random.generate CreateProject randomNameGenerator)

handleNewProject : Model -> String -> (Model, Cmd Msg)
handleNewProject model fileName =
    let
        jsonValue = JsonData.encodeProject (createNewProject model.newProjectDesc)
    in
    ({model | phase = Creating fileName}
    , apiCreateFile (FSCreate fileName jsonValue) ValidateProject model.apiCredentials)
    
handleUpdateAfterDelete : Model -> FileId -> (Model, Cmd Msg)
handleUpdateAfterDelete model fileId =
    let
        filteredArray = List.filter (\n -> n.fileId /= fileId) model.projects
        newModel = {model | projects = filteredArray}
        newJson = JsonData.encodeHome newModel.projects
    in
    ({newModel | phase = Updating newJson}, apiUpdateFile (FSUpdate model.homeId newJson) (UpdateInfo True) model.apiCredentials)

handleError : Model -> HomePhase -> Http.Error -> (Model, Cmd Msg)
handleError model phase error =
    ({model | phase = Failing (HomeError phase (httpErrorToString error))}, Cmd.none)

-- UPDATE

type alias StayHome = Bool

type Msg
    = Check (Result Http.Error (List InfoFile))
    | Retry HomeError
    | CreateInit (Result Http.Error String)
    | FillInfo (Result Http.Error ListProjects)
    | UpdateInfo StayHome (Result Http.Error String)
    | AddProject
    | OnNameChange String
    | OnDescChange String
    | CancelProject
    | CreateNameProject
    | CreateProject (List String)
    | ValidateProject (Result Http.Error String)
    | RemoveProject Validation ProjectInfo
    | CheckDeleteProject FileId (Result Http.Error (List InfoFile))
    | DeleteProject FileId (Result Http.Error ())
    | GoToProject (ProjectInfo, ApiCredentials)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Check result ->
            case result of
                Ok listFiles ->
                    if List.isEmpty listFiles then
                        handleInit model
                    else
                        let
                            filteredList = List.filter (\n -> n.name == "tracy.json") listFiles
                        in
                        if List.length filteredList == 1 then
                            let
                                tracy = case List.head filteredList of
                                    Just value -> value
                                    Nothing -> InfoFile "" ""
                            in
                            ({model | phase = Filling, homeId = tracy.fileId}
                            , apiReadFile (FSRead tracy.fileId) FillInfo JsonData.decodeHome model.apiCredentials)
                        else
                            handleInit model
                Err error -> handleError model Checking error
        Retry error ->
            case error.phase of
                Checking ->
                    ({model | phase = Checking}, apiGetListFiles FSNone Check model.apiCredentials)
                Initializing ->
                    ({model | phase = Checking}, apiGetListFiles FSNone Check model.apiCredentials)
                Filling ->
                    ({model | phase = Filling}
                    , apiReadFile (FSRead model.homeId) FillInfo JsonData.decodeHome model.apiCredentials)
                Creating existingName ->  handleNewProject model existingName
                Updating jsonValue ->
                    ({model | phase = Updating jsonValue}
                    , apiUpdateFile (FSUpdate model.homeId jsonValue) (UpdateInfo False) model.apiCredentials)
                Removing _ projectInfo ->
                    ({model | phase = Removing True projectInfo}
                    , apiGetListFiles FSNone (CheckDeleteProject projectInfo.fileId) model.apiCredentials)
                _ -> (model, Cmd.none)
        CreateInit result ->
            case result of
                Ok fileId ->
                    ({model | phase = Filling, homeId = fileId}
                    , apiReadFile (FSRead fileId) FillInfo JsonData.decodeHome model.apiCredentials)
                Err error -> handleError model Initializing error
        FillInfo result ->
            case result of
                Ok listProjects ->
                    let sortedProjects = List.sortBy .name listProjects in
                    ({model | projects = sortedProjects, phase = Listing False}, Cmd.none)
                Err error -> handleError model Filling error
        UpdateInfo stay result ->
            case result of
                Ok _ ->
                    case stay of
                        True -> ({model | phase = Listing False}, Cmd.none)
                        False ->
                            let
                                lastProject =
                                    case (List.head model.projects) of
                                        Just projectInfo -> projectInfo
                                        Nothing -> ProjectInfo "" "" [0,0,0]
                            in
                            (model, Task.perform GoToProject (Task.succeed (lastProject, model.apiCredentials)))
                Err error -> handleError model model.phase error
        AddProject -> ({model | phase = Listing True}, Cmd.none)
        OnNameChange name -> ({model | newProjectName = name}, Cmd.none)
        OnDescChange desc -> ({model | newProjectDesc = desc}, Cmd.none)
        CancelProject -> ({model | phase = Listing False}, Cmd.none)
        CreateNameProject -> handleNewNameProject model
        CreateProject generatedName -> handleNewProject model (String.concat generatedName)
        ValidateProject result ->
            case result of
                Ok fileId ->
                    let
                        projectInfo = ProjectInfo fileId model.newProjectName [0,0,0]
                        newArrayProjects = projectInfo::model.projects
                        newModel = {model | projects = newArrayProjects}
                        newJson = JsonData.encodeHome newModel.projects
                    in
                    ({newModel | newProjectName = "", newProjectDesc = "", phase = Updating newJson}
                    , apiUpdateFile (FSUpdate model.homeId newJson) (UpdateInfo False) model.apiCredentials)
                Err error -> handleError model model.phase error
        RemoveProject validation projectInfo ->
            case validation of
                False -> ({model | phase = Removing False projectInfo}, Cmd.none)
                True ->
                    ({model | phase = Removing True projectInfo}
                    , apiGetListFiles FSNone (CheckDeleteProject projectInfo.fileId) model.apiCredentials)
        CheckDeleteProject fileId result ->
            case result of
                Ok listFiles ->
                    let
                        filteredList = List.filter (\n -> n.fileId == fileId) listFiles
                    in
                    if List.isEmpty filteredList then
                        handleUpdateAfterDelete model fileId
                    else
                        (model, apiDeleteFile fileId (DeleteProject fileId) model.apiCredentials)
                Err error -> handleError model model.phase error
        DeleteProject fileId result ->
            case result of
                Ok _ -> handleUpdateAfterDelete model fileId
                Err error -> handleError model model.phase error
        GoToProject _ -> (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model  =
    div [class "core"]
        [ div [class "zone_status"]
            (case model.phase of
                Checking -> [text "Vérification ..."]
                Failing error ->
                    [ span [class "error"] [text "Il y a eu un problème !"]
                    , button [onClick (Retry error), class "button_topsnap"] [text "Réessayer"]
                    ]
                Initializing -> [text "Initialisation ..."]
                Filling -> [text "Importation ..."]
                Listing addStatus ->
                    case addStatus of
                        True -> [ span [] [text "Ajout d'un nouveau projet"] ]
                        False ->
                            [ span [] [text "Liste des Projets"]
                            , button [onClick AddProject, class "button_topsnap"] [iconAdd]
                            ]
                Creating _ -> [text "Création du projet en cours ..."]
                Updating _ -> [text "Mise à jour de la racine ..."]
                Removing _ _ -> [text "Suppression du projet ..."]
            )
        , (case model.phase of
            Failing error -> div [class "error"] [text error.info]
            Listing addStatus ->
                case addStatus of
                    True ->
                        div []
                            [ p []
                                [ label [for "project_name"] [text "Nom : "]
                                , input
                                    [ id "project_name"
                                    , value model.newProjectName
                                    , onInput OnNameChange
                                    , maxlength 20
                                    , style "width" "200px"
                                    ]
                                    []
                                ]
                            , p [] [label [for "project_desc"] [text "Description (optionnel) :"]]
                            , textarea
                                [id "project_desc", onInput OnDescChange, style "width" "90%", rows 7]
                                [text model.newProjectDesc]
                            , p [class "centered"]
                                [ button [onClick CancelProject, class "button_round"] [iconClose]
                                , button [onClick CreateNameProject, class "button_round"] [iconValid]
                                ]
                            ]
                    False ->
                        if List.isEmpty model.projects then
                            div [class "centered"] [text "Vous n'avez actuellement aucun projet"]
                        else
                            div [class "project_grid"]
                            (List.map (viewProject model.apiCredentials) model.projects)
            Removing step projectInfo ->
                case step of
                    False ->
                        div [class "centered"]
                            [ p [] [text "Êtes-vous sûr de vouloir supprimer le projet suivant :"]
                            , p [] [text projectInfo.name]
                            , button [onClick CancelProject, class "button_round"] [iconClose]
                            , button [onClick (RemoveProject True projectInfo), class "button_round"] [iconValid]
                            ]
                    True -> div [class "waiter"]
                        [ p [] [text "Suppression en cours,"]
                        , p [] [text "Veuillez patienter"]
                        ]
            _ -> div [class "waiter"] [text "Veuillez patienter"]
        )
        ]

viewProject : ApiCredentials -> ProjectInfo -> Html Msg
viewProject credentials projectInfo =
    let
        valuesDict = Dict.fromList (List.indexedMap Tuple.pair projectInfo.values)
        total = List.sum projectInfo.values
        getValue id = case (Dict.get id valuesDict) of
            Just value -> String.fromInt value
            Nothing -> "0"
    in
    div [class "project_box"]
        [ a [onClick (GoToProject (projectInfo, credentials))] [text projectInfo.name]
        , button [onClick (RemoveProject False projectInfo), class "button_round"] [iconClose]
        , div [class "project_progress"]
            [ span [class "round_box done_color"] [text (getValue 0)]
            , span [class "round_box wip_color"] [text (getValue 1)]
            , span [class "round_box wait_color"] [text (getValue 2)]
            , span [class "round_box total_color"] [text (String.fromInt total)]
            ]
        , (if total > 0 then
            --
            -- TODO : insérer un diagramme en barres sommées
            --
            div [] [text "(future barre ici"]
            --
        else text "")
        , p [] [text "(Statut du projet, à venir)"]
        ]
