port module Main exposing (..)

import Api exposing (..)
import Types exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, h1, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Json.Decode exposing (Decoder, decodeValue, errorToString, field, int, list, string)
import Json.Encode as JE


-- WIP : lecture du fichier tracy.json
-- TODO : listing des projets
-- TODO : ajouter des projets
-- TODO : supprimer des projets
-- TODO : vérifier qu'il y a autant de fichiers de projets que de projets, et faire le ménage si besoin

-- TODO : icone pour la connexion
-- TODO : icone pour la déconnexion

-- TODO : page d'accueil d'un projet, avec son nom et sa description
-- TODO : faire en sorte de pouvoir modifier le nom et la description

-- TODO : page de listing des actions à mener

-- MAIN

main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- JSON ENCODE

-- TODO : encoder pour les metadata et le format des fichiers créés / uploadés

-- JSON DECODE

decodeAnswer : JE.Value -> Answer
decodeAnswer value =
    let
        checkStatus = decodeValue (field "status" string) value
    in
    case checkStatus of
        Err error -> AError ("checkStatus error: "++(errorToString error))
        Ok status ->
            case status of
                "AError" ->
                    let
                        checkDesc = decodeValue (field "desc" string) value
                    in
                    case checkDesc of
                        Err error -> AError ("checkDesc error: "++(errorToString error))
                        Ok desc -> AError desc
                "Connected" -> Connected (decodeValue (field "token" string) value)
                "Unconnected" -> Unconnected
                _ -> AError "Something went wrong, bad value for status"

type alias InfoFile =
    { fileId : FileId
    , name : String
    }

decodeInfoFile : Decoder InfoFile
decodeInfoFile =
    JD.map2 InfoFile (field "id" string) (field "name" string)

decodeListFiles : Decoder (List InfoFile)
decodeListFiles =
    field "files" (list decodeInfoFile)

decodeCreateFile : Decoder String
decodeCreateFile =
    field "id" string

type alias ListProjects = List InfoFile

decodeReadHome : Decoder (List InfoFile)
decodeReadHome =
    field "projects" (list decodeInfoFile)

decodeReadProject : Decoder String
decodeReadProject =
    --
    -- TODO : décodage du json pour un projet
    --
    field "name" string
    --

--
-- TODO : a supprimer dès que possible
--
decodeTest : Decoder (List String)
decodeTest =
    field "files" (list string)



-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Asking question ->
            case question of
                AskInit -> (model, Cmd.none)
                AskLogIn -> (modelChangeStatus model Connecting LogIn, ask "Authenticate")
                AskLogOut -> ({model | withErr = ErrNo}, ask "SignOut")
        ReceptionData value ->
            let
                answer = decodeAnswer value
            in
            case answer of
                AError info -> ({model | status = model.prevStatus, withErr = ErrYes info}, Cmd.none)
                Unconnected -> ({model | status = LogIn}, Cmd.none)
                Connected resToken ->
                    case resToken of
                        Err error ->
                            (modelGotError {model | status = Home HomeError} (errorToString error), Cmd.none)
                        Ok token ->
                            ( modelChangeStatus {model | token = token} (Home HomeCheck) (Home HomeError)
                            , makeRequest ListFiles FSNone token model.api_key)
        GetListFiles result ->
            case result of
                Ok listFiles ->
                    if (List.length listFiles) == 0 then
                        ( modelChangeStatus model (Home HomeInit) (Home HomeCheck)
                        , makeRequestModel CreateFile (FSCreate "tracy" False) model)
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
                            ( modelChangeStatus {model | homeId = tracy.fileId} (Home HomeGetInfo) (Home HomeError)
                            , makeRequestModel ReadFile (FSRead tracy.fileId False) model)
                        else
                            ( modelChangeStatus model (Home HomeInit) (Home HomeCheck)
                            , makeRequestModel CreateFile (FSCreate "tracy" False) model)
                Err error -> (modelGotError model (httpErrorToString error), Cmd.none)
        HomeRetry ->
            case model.prevStatus of
                Home phase ->
                    ( modelChangeStatus model model.prevStatus model.prevStatus
                    , case phase of
                        HomeCheck -> makeRequestModel ListFiles FSNone model
                        HomeInit -> makeRequestModel CreateFile (FSCreate "tracy" False) model
                        HomeGetInfo -> makeRequestModel ReadFile (FSRead model.homeId False) model
                        --
                        -- TODO : faire les autres cas
                        --
                        _ -> Cmd.none
                        --
                    )
                _ -> (model, Cmd.none)
        GetCreateFile result ->
            case result of
                Ok createdFile ->
                    ( modelChangeStatus model (Home HomeCheck) (Home HomeError)
                    , makeRequestModel ListFiles FSNone model)
                Err error -> (modelGotError model (httpErrorToString error), Cmd.none)
        GetReadHome result ->
            case result of
                Ok fileRead -> ({model | projects = fileRead, status = (Home HomeList)}, Cmd.none)
                Err error -> (modelGotError model (httpErrorToString error), Cmd.none)
        GetReadProject result ->
            case result of
                Ok fileRead ->
                    --
                    -- TODO : mettre à jour les infos sur le projet
                    --
                    (model, Cmd.none)
                    --
                Err error -> (modelGotError model (httpErrorToString error), Cmd.none)
        AddProject ->
            --
            -- TODO : préparer l'affichage pour créer un nouveau projet
            --
            (model, Cmd.none)
            --
        CancelProject ->
            --
            -- TODO : annuler la création d'un projet
            --
            (model, Cmd.none)
            --
        CreateProject ->
            --
            -- TODO : valider la création d'un projet (envoi de la demande de création, etc)
            --
            (model, Cmd.none)
            --
        --
        --
        -- TODO : ajouter ici les autres messages
        --
        -- TODO : après cette ligne ce trouve des fonctions de tests qui devraient disparaître à terme
        --
        TestList ->
            (model, makeRequestModel ListFiles FSNone model)
        TestCreate ->
            (model, makeRequestModel CreateFile (FSCreate model.testValue True) model)
        TestRead ->
            (model, makeRequestModel ReadFile (FSRead model.testValue True) model)
        TestDelete ->
            (model, makeRequestModel DeleteFile (FSDelete model.testValue) model)
        TestRep rep -> (model, Cmd.none)
        TestChange value -> ({model | testValue = value}, Cmd.none)

-- PORTS

port ask : String -> Cmd msg
port received : (JE.Value -> msg) -> Sub msg

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
    received ReceptionData

-- VIEW

view : Model -> Browser.Document Msg
view model =
    let
        deco_btn = if model.status /= LogIn && model.status /= Connecting then
            [ div [class "panel_deconnect"] [button
                [onClick (Asking AskLogOut), class "button_topsnap"]
                [text "Déconnexion"]
                ]
            ]
            else []
    in
    { title = "Tracy"
    , body = List.append deco_btn [
        case model.status of
            Connecting -> div [class "core waiter"] [text "Connexion en cours ..."]
            LogIn -> viewLogIn model.withErr
            Home phase -> viewHome model phase
            Project phase -> viewProject model phase
    ]}

viewError : ShowErr -> Html Msg
viewError withErr =
    case withErr of
        ErrYes info -> div [class "error"] [text info]
        ErrNo -> div [class "hide"] []

viewLogIn : ShowErr -> Html Msg
viewLogIn withErr =
    div [class "panel_connect"]
        [button [onClick (Asking AskLogIn), class "button"] [text "Connexion"]
        , viewError withErr
        ]

viewHome : Model -> HomePhase -> Html Msg
viewHome model phase =
    div [class "core"]
        [ div [class "zone_status"]
            (case phase of
                HomeCheck -> [text "Vérification ..."]
                HomeError ->
                    [ span [class "error"] [text "Il y a eu un problème !"]
                    , button [onClick HomeRetry, class "button"] [text "Réessayer"]
                    ]
                HomeInit -> [text "Initialisation ..."]
                HomeGetInfo -> [text "Importation ..."]
                HomeList ->
                    [ span [] [text "Liste des Projets"]
                    --
                    , button [onClick AddProject] [text "Ajouter"]
                    --
                    ]
                    --
                    -- TODO : lister tous les projets
                    --
                    -- TODO : d'abord les boutons
                    --
                    -- TODO : puis la liste des projets
                    --
                --
                -- TODO : ajouter les cas manquant
                --
                _ -> [text "another status"]
                --
            )
        , viewError model.withErr
        , div [] [
            --
            -- TODO : la liste des projets
            --
            -- TODO : test avec HTTP from elm
            --
            button [onClick TestList] [text "List"]
            , button [onClick TestRead] [text "Read"]
            , button [onClick TestCreate] [text "Create"]
            , input [value model.testValue, onInput TestChange, style "width" "250px"] []
            , button [onClick TestDelete] [text "Delete"]
            --
        ]
        ]

viewProject : Model -> ProjectPhase -> Html Msg
viewProject model phase =
    div [class "core"]
        --
        [text "page d'un projet"]
        --
