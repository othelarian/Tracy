module Home exposing(Model, Msg(..), init, update, view)

import Api exposing (..)
import Project exposing (ProjectInfo, encodeNewProject, init)

import Array exposing (Array)
import Html exposing (Html, button, div, input, label, p, span, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Task
import Time

-- MODEL

type alias HomeError =
    { phase : HomePhase
    , info : String
    }

type alias Adding = Bool

type HomePhase
    = Checking
    | Failing HomeError
    | Initializing
    | Filling
    | Listing Adding
    | Creating String
    | Updating JE.Value 
    | Removing

type alias ArrayProjects = Array InfoFile

type alias Model =
    { apiCredentials : ApiCredentials
    , homeId : FileId
    , projects : ArrayProjects
    , phase : HomePhase
    , newProjectName : String
    , newProjectDesc : String
    --
    --
    , testValue : String
    --
    }

init : ApiCredentials -> Model
init apiCredentials =
    Model apiCredentials "" Array.empty Checking "" "" ""

-- JSON DECODE

decodeReadHome : JD.Decoder ArrayProjects
decodeReadHome =
    JD.field "projects" (JD.array decodeInfoFile)

-- JSON ENCODE

encodeHome : Model -> JE.Value
encodeHome model =
    let
        encodeInfoFile : InfoFile -> JE.Value
        encodeInfoFile infoFile =
            JE.object
                [ ("id", JE.string infoFile.fileId)
                , ("name", JE.string infoFile.name)
                ]
    in
    JE.object [ ("projects", JE.array encodeInfoFile model.projects) ]

-- HANDLERS

handleInit : Model -> (Model, Cmd Msg)
handleInit model =
    let
        jsonValue = JE.object [ ("projects", JE.array JE.string Array.empty) ]
    in
    ({model | phase = Initializing}
    , apiCreateFile (FSCreate "tracy" jsonValue) CreateInit model.apiCredentials)

handleNewNameProject : String
handleNewNameProject =
    let
        getMonthNumber : Time.Month -> String
        getMonthNumber month =
            case month of
                Time.Jan -> "01"
                Time.Feb -> "02"
                Time.Mar -> "03"
                Time.Apr -> "04"
                Time.May -> "05"
                Time.Jun -> "06"
                Time.Jul -> "07"
                Time.Aug -> "08"
                Time.Sep -> "09"
                Time.Oct -> "10"
                Time.Nov -> "11"
                Time.Dec -> "12"
        getDoubleDigit : Int -> String
        getDoubleDigit number =
            if number < 9 then "0"++(String.fromInt number) else String.fromInt number
        time = Time.millisToPosix 0
        timeName =
            (String.fromInt (Time.toYear Time.utc time))
            ++ (getMonthNumber (Time.toMonth Time.utc time))
            ++ (getDoubleDigit (Time.toDay Time.utc time))
            ++ (getDoubleDigit (Time.toHour Time.utc time))
            ++ (getDoubleDigit (Time.toMinute Time.utc time))
            ++ (getDoubleDigit (Time.toSecond Time.utc time))
    in
    "project_"++timeName

handleNewProject : Model -> Maybe String -> (Model, Cmd Msg)
handleNewProject model maybeName =
    let
        newFileName = case maybeName of
            Just existingName -> existingName
            Nothing -> handleNewNameProject
        jsonValue = encodeNewProject model.newProjectName model.newProjectDesc
    in
    ({model | phase = Creating newFileName}
    , apiCreateFile (FSCreate newFileName jsonValue) ValidateProject model.apiCredentials)
    

handleError : Model -> HomePhase -> Http.Error -> (Model, Cmd Msg)
handleError model phase error =
    ({model | phase = Failing (HomeError phase (httpErrorToString error))}, Cmd.none)

-- UPDATE

type alias StayHome = Bool

type Msg
    = Check (Result Http.Error (Array InfoFile))
    | Retry HomeError
    | CreateInit (Result Http.Error String)
    | FillInfo (Result Http.Error ArrayProjects)
    --
    | UpdateInfo StayHome (Result Http.Error String)
    --
    | AddProject
    | OnNameChange String
    | OnDescChange String
    | CancelProject
    | CreateProject
    | ValidateProject (Result Http.Error String)
    --
    | OpenProject
    --
    | RemoveProject
    --
    | GoToProject (FileId, ApiCredentials)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Check result ->
            case result of
                Ok listFiles ->
                    if Array.isEmpty listFiles then
                        handleInit model
                    else
                        let
                            filteredArray = Array.filter (\n -> n.name == "tracy.json") listFiles
                        in
                        if Array.length filteredArray == 1 then
                            let
                                tracy = case Array.get 0 filteredArray of
                                    Just value -> value
                                    Nothing -> InfoFile "" ""
                            in
                            ({model | phase = Filling, homeId = tracy.fileId}
                            , apiReadFile (FSRead tracy.fileId) FillInfo decodeReadHome model.apiCredentials)
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
                    , apiReadFile (FSRead model.homeId) FillInfo decodeReadHome model.apiCredentials)
                Creating existingName ->  handleNewProject model (Just existingName)
                Updating jsonValue ->
                    ({model | phase = Updating jsonValue}
                    , apiUpdateFile (FSUpdate model.homeId jsonValue) (UpdateInfo False) model.apiCredentials)
                Removing ->
                    --
                    -- TODO : gestion du cas de suppression qui n'a pas fonctionné
                    --
                    (model, Cmd.none)
                    --
                    --
                _ -> (model, Cmd.none)
        CreateInit result ->
            case result of
                Ok fileId ->
                    ({model | phase = Filling, homeId = fileId}
                    , apiReadFile (FSRead fileId) FillInfo decodeReadHome model.apiCredentials)
                Err error -> handleError model Initializing error
        FillInfo result ->
            case result of
                Ok listProjects ->
                    --
                    -- TODO : sorting the list
                    --
                    ({model | projects = listProjects, phase = Listing False}, Cmd.none)
                Err error -> handleError model Filling error
        UpdateInfo stay result ->
            case result of
                Ok _ ->
                    case stay of
                        True -> ({model | phase = Listing False}, Cmd.none)
                        False ->
                            let
                                lastIndex = (Array.length model.projects) - 1
                                lastProject =
                                    case (Array.get lastIndex model.projects) of
                                        Just infoFile -> infoFile
                                        Nothing -> InfoFile "" ""
                            in
                            (model, Task.perform GoToProject (Task.succeed (lastProject.fileId, model.apiCredentials)))
                Err error -> handleError model model.phase error
        AddProject -> ({model | phase = Listing True}, Cmd.none)
        OnNameChange name -> ({model | newProjectName = name}, Cmd.none)
        OnDescChange desc -> ({model | newProjectDesc = desc}, Cmd.none)
        CancelProject -> ({model | phase = Listing False}, Cmd.none)
        CreateProject -> handleNewProject model Nothing
        ValidateProject result ->
            case result of
                Ok fileId ->
                    let
                        infoFile = InfoFile fileId model.newProjectName
                        newArrayProjects = Array.push infoFile model.projects
                        newModel = {model | projects = newArrayProjects}
                        jsonValue = encodeHome newModel
                    in
                    ({newModel | newProjectName = "", newProjectDesc = "", phase = Updating jsonValue}
                    , apiUpdateFile (FSUpdate model.homeId jsonValue) (UpdateInfo False) model.apiCredentials)
                Err error -> handleError model model.phase error
        OpenProject ->
            --
            -- TODO : ouverture d'un projet lors d'un clic sur son nom
            --
            (model, Cmd.none)
            --
        RemoveProject ->
            --
            -- TODO : comment faire pour la suppression ?
            --
            (model, Cmd.none)
            --
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
                            , button [onClick AddProject, class "button_topsnap"] [text "Ajouter"]
                            ]
                Creating _ -> [text "Création du projet en cours ..."]
                Updating _ -> [text "Mise à jour de la racine ..."]
                Removing -> [text "Suppression du projet ..."]
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
                                    [id "project_name", onInput OnNameChange, maxlength 20, style "width" "200px"]
                                    [text model.newProjectName]
                                ]
                            , p [] [label [for "project_desc"] [text "Description (optionnel) :"]]
                            , textarea
                                [id "project_desc", onInput OnDescChange, style "width" "90%", rows 7]
                                [text model.newProjectDesc]
                            , p [style "text-align" "center"]
                                [ button [onClick CancelProject, class "button"] [text "Annuler"]
                                , button [onClick CreateProject, class "button"] [text "Valider"]
                                ]
                            ]
                    False ->
                        if Array.isEmpty model.projects then
                            div [class "waiter"] [text "Vous n'avez actuellement aucun projet"]
                        else
                            --
                            -- TODO : lister les projets
                            --
                            div [] [text "(liste des projets à venir"]
                            --
                            --
            _ -> div [class "waiter"] [text "Veuillez patienter"]
        )
        ]
