port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Bytes.Encode as BE
import Html exposing (Html, button, div, h1, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import Json.Decode exposing (Decoder, decodeValue, errorToString, field, int, list, string)
import Json.Encode as JE
import Url
import Url.Builder as UB


-- WIP : faire l'initialisation si besoin
-- TODO : listing des projets
-- TODO : ajouter des projets
-- TODO : supprimer des projets
-- TODO : vérifier qu'il y a autant de fichiers de projets que de projets, et faire le ménage si besoin

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

-- MODEL

type HomePhase
    = HomeCheck
    | HomeError
    --
    -- TODO : finir les phases
    --
    | HomeInit
    --
    | HomeList NeedClean
    --
    | HomeClean
    --
    --| HomeAdd
    --| HomeRemove
    --

type Status
    = Connecting
    | LogIn
    | Home HomePhase
    --
    -- TODO : project est en cours de construction
    --
    | Project
    --

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

type alias ApiKey = String

type alias FileId = String

type alias NeedClean = Bool

type alias Token = String

type alias Model =
    { status : Status
    , prevStatus : Status
    , api_key : ApiKey
    , token : Token
    , withErr : ShowErr
    }

modelChangeStatus : Model -> Status -> Status -> Model
modelChangeStatus model actual previous =
    Model actual previous model.api_key model.token ErrNo

modelGotError : Model -> String -> Model
modelGotError model error =
    case model.status of
        Connecting -> {model | status = LogIn, withErr = ErrYes error}
        LogIn -> model -- cas impossible
        Home phase ->
            case phase of
                HomeCheck ->
                    {model | status = Home HomeError, prevStatus = Home HomeCheck, withErr = ErrYes error}
                HomeError ->
                    {model | prevStatus = Home HomeError, withErr = ErrYes error}
                --
                -- TODO : faire les autres cas
                --
                _ -> model
                --
        Project ->
            --
            -- TODO : finaliser cette partie
            --
            model
            --

init : ApiKey -> (Model, Cmd Msg)
init apiKey =
    (Model Connecting LogIn apiKey "" ErrNo, ask "Starting")

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

decodeListFiles : Decoder (List InfoFile)
decodeListFiles =
    field "files" (list (JD.map2 InfoFile (field "id" string) (field "name" string)))



decodeTest : Decoder (List String)
decodeTest =
    field "files" (list string)



-- HTTP

httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl badUrl -> ("Vous avez tentez d'appeler ça ? vraiment ? -> "++badUrl)
        Http.Timeout -> "Vous avez pris trop de temps ..."
        Http.NetworkError -> "Il y a un truc avec le réseau ..."
        Http.BadStatus status -> ("Il y a un soucis, voici le code : "++(String.fromInt status))
        Http.BadBody body -> ("Vous avez reçu un message étonnant :\n"++body)

type HttpAction
    = ListFiles
    | CreateFile
    | ReadFile
    | UpdateFile
    | DeleteFile

type FileSelector
    = FSNone
    | Tracy
    | FileId String

makeRequestModel : HttpAction -> FileSelector -> Model -> Cmd Msg
makeRequestModel action selector model =
    makeRequest action selector model.token model.api_key

makeRequest : HttpAction -> FileSelector -> Token -> ApiKey -> Cmd Msg
makeRequest action selector token apiKey =
    Http.request
        { method = case action of
            CreateFile -> "POST"
            --
            UpdateFile -> "" -- TODO : TEST à faire, normalement "PATCH"
            --
            DeleteFile -> "DELETE"
            _ -> "GET"
        , headers = [(Http.header "authorization" ("Bearer "++token))]
        , url = (UB.crossOrigin
                "https://www.googleapis.com"
                (let
                    path = ["drive", "v3", "files"]
                    fileId = case selector of
                        FileId id -> id
                        _ -> ""
                in
                case action of
                    ListFiles -> path
                    CreateFile -> "upload"::path
                    UpdateFile -> List.append ("upload"::path) [fileId]
                    _ -> List.append path [fileId]
                )
                (let
                    key = [UB.string "key" apiKey]
                in
                case action of
                    ListFiles -> (UB.string "spaces" "appDataFolder")::key
                    --
                    -- TODO : ajouter les élément manquant, si besoin
                    --
                    _ -> []
                )
            )
        , body = case action of
            --
            --
            --
            -- TODO : CreateFile et UpdateFile
            --
            _ -> Http.emptyBody
        , expect = case action of
            ListFiles -> Http.expectJson GetListFiles decodeListFiles
            --
            --
            -- TODO : gérer tout les cas
            --
            _ -> Http.expectJson TestRep decodeTest
            --
        , timeout = Nothing
        , tracker = Nothing
        }

-- UPDATE

type Msg
    = Asking Question
    | ReceptionData JE.Value
    | GetListFiles (Result Http.Error (List InfoFile))
    | HomeRetry
    --
    -- TODO : la liste des messages continue
    --
    | TestList
    | TestCreate
    | TestDelete
    | TestRep (Result Http.Error (List String))
    --

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
                        --
                        -- TODO : initialisation
                        --
                        ( modelChangeStatus model (Home HomeInit) (Home HomeInit)
                        , makeRequestModel CreateFile Tracy model)
                        --
                    else
                        --
                        -- TODO : filtrer la liste, et s'il n'y a pas de tracy.json, alors on part en init
                        --
                        (model, Cmd.none)
                        --
                        --
                Err error -> (modelGotError model (httpErrorToString error), Cmd.none)
        HomeRetry ->
            case model.prevStatus of
                Home phase ->
                    ( modelChangeStatus model model.prevStatus model.prevStatus
                    , case phase of
                        HomeCheck -> makeRequestModel ListFiles FSNone model
                        --
                        --
                        -- TODO : faire les autres cas
                        --
                        _ -> Cmd.none
                        --
                    )
                _ -> (model, Cmd.none)
            --
            -- TODO : ajouter ici les autres messages
            --
            -- TODO : après cette ligne ce trouve des fonctions de tests qui devraient disparaître à terme
            --
        TestList ->
            (model, makeRequest ListFiles FSNone model.token model.api_key)
            --
        TestCreate ->
            --
            (model
            , Http.request
                { method = "POST"
                , headers = [(Http.header "authorization" ("Bearer "++model.token))]
                , url = ("https://www.googleapis.com/upload/drive/v3/files?uploadType=multipart&alt=json&key="++model.api_key)
                , body = Http.multipartBody
                    [ Http.bytesPart
                        "Metadata"
                        "application/json"
                        (BE.encode (BE.string "{'parents':['appDataFolder'],'name':'test123.json','mimeType':'application/json'}"))
                        --Http.stringPart "Metadata" "{'parents':['appDataFolder'],'name':'test123.json','mimeType':'application/json'}"
                    --, Http.stringPart "Media" "{'test': 'value'}"
                    , Http.bytesPart
                        "Media"
                        "application/json"
                        (BE.encode (BE.string "{'test': 'value'}"))
                    ]
                , expect = Http.expectJson TestRep (list string)
                , timeout = Nothing
                , tracker = Nothing
                }
            )
            --
        TestDelete ->
            (model
            , Http.request
                { method = "DELETE"
                , headers = [
                    (Http.header "authorization" ("Bearer "++model.token))
                    ]
                , url = (Url.toString (Url.Url
                    Url.Https
                    "www.googleapis.com"
                    Nothing
                    "/drive/v3/files/1KXH4ZYiI74fyMC0z-IUzc6oMkoHr--vecD4sMjTiqhOSCW_R6w"
                    (Just ("key="++model.api_key))
                    Nothing
                ))

                --"https://www.googleapis.com/drive/v3/files/1xo_US3LixGC5uuTSkzhza-lEakl63-JkKkDwCUOLhS9xHOw6xA"
                , body = Http.emptyBody
                , expect = Http.expectJson TestRep (list string)
                , timeout = Nothing
                , tracker = Nothing
                }
            )
            --
        TestRep rep -> (model, Cmd.none)
            --

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
    { title = "Tracy"
    , body = [
        --h1 [] [text "Tracy"]
        --
        --
        --div [] [text ("API_KEY: "++model.api_key)]
        --, div [] [text ("token: "++model.token)]
        --,
        --
        case model.status of
            Connecting -> div [class "core waiter"] [text "Connexion en cours ..."]
            LogIn -> viewLogIn model.withErr
            Home phase -> viewHome model phase
            --
            Project -> viewProject
            --
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
        [ div [class "panel_deconnect"]
            [button
                [onClick (Asking AskLogOut), class "button"]
                [text "Déconnexion"]
            ]
        , div [class "zone_status"]
            (case phase of
                HomeCheck -> [text "Vérification ..."]
                HomeError ->
                    [span [class "error"] [text "Il y a eu un problème !"]
                    , button [onClick HomeRetry, class "button"] [text "Réessayer"]
                    ]
                --
                HomeInit -> [text "Initialisation ..."]
                --
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
            , button [onClick TestCreate] [text "Create"]
            , button [onClick TestDelete] [text "Delete"]
            --
        ]
        ]

viewProject : Html Msg
viewProject =
    div [class "core"]
        --
        [text "page d'un projet"]
        --
