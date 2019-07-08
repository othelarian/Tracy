module Home exposing(Model, Msg(..), init, update, view)

import Api exposing (..)
import Project exposing (ProjectInfo, encodeNewProject)

import Array exposing (Array)
import Html exposing (Html, button, div, input, label, p, span, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE
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
    | Updating
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
    --
    -- TODO : finaliser l'encodage en json du home
    --
    JE.object [ ("projects", JE.array (JD.map2 InfoFile (field "id" string) (field "name" string)) model.projects) ]

-- HANDLERS

handleInit : Model -> (Model, Cmd Msg)
handleInit model =
    let
        jsonValue = JE.object [ ("projects", JE.array JE.string []) ]
    in
    ({model | phase = Initializing}
    , apiCreateFile (FSCreate "tracy" jsonValue) CreateInit model.apiCredentials)

handleNewProject : Model -> (Model, Cmd Msg)
handleNewProject model =
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
        newName = "project_"++timeName
        jsonValue = encodeNewProject model.newProjectName model.newProjectDesc
    in
    ({model | phase = Creating}
    , apiCreateFile (FSCreate newName jsonValue) ValidateProject model.apiCredentials)
    

handleError : Model -> HomePhase -> Http.Error -> (Model, Cmd Msg)
handleError model phase error =
    ({model | phase = Failing (HomeError phase (httpErrorToString error))}, Cmd.none)

-- UPDATE

type Msg
    = Check (Result Http.Error (Array InfoFile))
    | Retry HomeError
    | CreateInit (Result Http.Error String)
    | FillInfo (Result Http.Error ArrayProjects)
    --
    | UpdateInfo (Result Http.Error String)
    --
    | AddProject
    | OnNameChange String
    | OnDescChange String
    | CancelProject
    | CreateProject
    | ValidateProject (Result Http.Error String)
    --
    | RemoveProject
    --

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
                --
                Creating _ ->
                    --
                    -- TODO : valider l'idée que c'est le create qui plante
                    --
                    (model, Cmd.none)
                    --
                --
                -- TODO : il faut pouvoir réessayer d'autre phase
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
        UpdateInfo result ->
            --
            -- TODO : valider la mise à jour de tracy.json
            --
            -- TODO : ouvrir le projet avec la vue projet si c'est une création
            --
            (model, Cmd.none)
            --
        AddProject -> ({model | phase = Listing True}, Cmd.none)
        OnNameChange name -> ({model | newProjectName = name}, Cmd.none)
        OnDescChange desc -> ({model | newProjectDesc = desc}, Cmd.none)
        CancelProject -> ({model | phase = Listing False}, Cmd.none)
        CreateProject -> handleNewProject model
        ValidateProject result ->
            case result of
                Ok fileId ->
                    let
                        infoFile = InfoFile fileId model.newProjectName
                        newArrayProjects = Array.push infoFile model.projects
                    in
                    --
                    -- TODO : sauvegarder la création du projet dans tracy.json
                    --
                    ({model | projects = newArrayProjects, newProjectName = "", newProjectDesc = ""}
                    , apiUpdateFile (FSUpdate model.homeId ) UpdateInfo model.apiCredentials)
                    --
                Err error -> handleError model model.phase error
        RemoveProject ->
            --
            -- TODO : comment faire pour la suppression ?
            --
            (model, Cmd.none)
            --

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
