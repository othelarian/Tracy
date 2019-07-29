port module Main exposing (..)

import Api exposing (..)
import Graphics exposing (iconExit)
import Home
import JsonData
import Project

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, h1, input, p, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Extra as Html
import Http
import Json.Decode as JD
import Json.Decode exposing (Decoder, array, decodeValue, errorToString, field, int, string)
import Json.Encode as JE
import Time
import Url
import Url.Parser as UP
import Url.Parser.Query as UPQ

-- MAIN

main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = (\_ -> NoOp)
        , onUrlRequest = (\_ -> NoOp)
        }

-- MODEL

type GuestPhase
    = Checking
    | Panic
    | Login
    | Refreshing
    | DriveOff String
    | Saving
    | Connecting
    | Failed String

type alias GuestModel =
    { phase : GuestPhase
    , apiToken : ApiToken
    , url : Url.Url
    , testValue : String
    , testShow : Bool
    }

guestModel : Url.Url ->  GuestModel
guestModel url =
    GuestModel Checking (ApiToken "" "" 0) url "" False

type Model
    = Guest GuestModel
    | Home Home.Model
    | Project Project.Model

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
    let
        getCode =
            case UP.parse (UP.query (UPQ.string "code")) {url | path = ""} of
                Just res -> res
                Nothing -> Nothing
        newModel = guestModel url
    in
    case getCode of
        Just code ->
            let newUrl = {url | query = Nothing, fragment = Nothing} in
            ( Guest {newModel | phase = Refreshing, url = newUrl}
            , Cmd.batch [Nav.replaceUrl key (Url.toString newUrl), getFirstAccess code newUrl GetFirstAccess])
        Nothing ->
            (Guest newModel, Http.get {url = "/getToken", expect = Http.expectString Check})

-- HANDLERS

type alias GenericModel =
    { apiToken : ApiToken
    , url : Url.Url
    , testValue : String
    , testShow : Bool
    }

getGenericModel : Model -> GenericModel
getGenericModel model =
    case model of
        Guest guest -> GenericModel guest.apiToken guest.url guest.testValue guest.testShow
        Home home -> GenericModel home.apiToken home.url home.testValue home.testShow
        Project project -> GenericModel project.apiToken project.url project.testValue project.testShow

-- UPDATE

type Msg
    = NoOp
    | Check (Result Http.Error String)
    | AskLogIn
    | AskFirstAccess
    | GetFirstAccess (Result Http.Error ApiToken)
    | SavedToken (Result Http.Error String)
    | AskRefresh Time.Posix
    | GetRefresh (Result Http.Error ApiToken)
    | HomeMsg Home.Msg
    | ProjectMsg Project.Msg
    | TestCom String
    | TestShow
    | TestList
    | TestCreate
    | TestRead
    | TestDelete
    | TestRepList (Result Http.Error (List InfoFile))
    | TestRepString (Result Http.Error String)
    | TestRepValue (Result Http.Error String)
    | TestRepUnit (Result Http.Error ())
    | TestChange String

updateWith : (subModel -> Model) -> (subMsg -> Msg) -> (subModel, Cmd subMsg) -> (Model, Cmd Msg)
updateWith toModel toMsg (subModel, subCmd) =
    (toModel subModel, Cmd.map toMsg subCmd)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model) of
        (NoOp, _) -> (model, Cmd.none)
        (Check rep, Guest guest) ->
            case rep of
                Ok token ->
                    case token of
                        "failed" -> (Guest {guest | phase = Panic}, Cmd.none)
                        "" -> (Guest {guest | phase = Login}, Cmd.none)
                        _ -> (Guest {guest | phase = Refreshing}, getRefreshAccess token GetRefresh)
                Err _ -> (model, Cmd.none)
        (AskLogIn, _) ->
            let genModel = getGenericModel model in
            (model, Nav.load (createIdentityUrl genModel.url))
        (GetFirstAccess res, Guest guest) ->
            case res of
                Ok apiToken ->
                    let
                        modelUrl = guest.url
                        newUrl = {modelUrl | path = "saveToken", query = Just("token="++apiToken.refreshToken), fragment = Nothing}
                    in
                    ( Guest {guest | apiToken = apiToken, phase = Saving}
                    , Http.get
                        { url = Url.toString newUrl
                        , expect = Http.expectString SavedToken
                        }
                    )
                Err error -> (Guest {guest | phase = DriveOff (httpErrorToString error)}, Cmd.none)
        (SavedToken res, Guest guest) ->
            case res of
                Ok rep ->
                    case rep of
                        "saved" ->
                            ( Home (Home.init guest.apiToken guest.url guest.testShow)
                            , Cmd.map HomeMsg (getListFiles FSNone Home.Check guest.apiToken.accessToken))
                        "failed" -> (Guest {guest | phase = Panic}, Cmd.none)
                        _ -> (model, Cmd.none)
                Err error -> (Guest {guest | phase = Panic}, Cmd.none)
        (AskRefresh _, _) ->
            let genModel = getGenericModel model in
            (model, getRefreshAccess genModel.apiToken.refreshToken GetRefresh)
        (GetRefresh res, _) ->
            case res of
                Ok apiToken ->
                    case model of
                        Guest guest ->
                            ( Home (Home.init apiToken guest.url guest.testShow)
                            , Cmd.map HomeMsg (getListFiles FSNone Home.Check apiToken.accessToken))
                        Home home -> (Home {home | apiToken = apiToken}, Cmd.none)
                        Project project -> (Project {project | apiToken = apiToken}, Cmd.none)
                Err error ->
                    let
                        genModel = getGenericModel model
                        newModel = guestModel genModel.url
                    in
                    (Guest {newModel | phase = DriveOff (httpErrorToString error)}, Cmd.none)
        (HomeMsg subMsg, Home home) ->
            case subMsg of
                Home.GoToProject (projectInfo, apiToken) ->
                    (Project (Project.init apiToken home.url projectInfo home.homeId home.testShow)
                    , Cmd.map
                        ProjectMsg
                        (readFile
                            (FSRead projectInfo.fileId)
                            (Project.LoadProject projectInfo)
                            JsonData.decodeProject
                            home.apiToken.accessToken
                        )
                    )
                _ -> updateWith Home HomeMsg (Home.update subMsg home)
        (ProjectMsg subMsg, Project project) ->
            case subMsg of
                Project.GoToHome apiToken ->
                    (Home (Home.init apiToken project.url project.testShow)
                    , Cmd.map HomeMsg (getListFiles FSNone Home.Check project.apiToken.accessToken))
                _ -> updateWith Project ProjectMsg (Project.update subMsg project)
        (TestCom testMsg, _) ->
            let genModel = getGenericModel model in
            case testMsg of
                "toggle" ->
                    case model of
                        Guest data -> (Guest {data | testShow = not genModel.testShow}, Cmd.none)
                        Home data -> (Home {data | testShow = not genModel.testShow}, Cmd.none)
                        Project data -> (Project {data | testShow = not genModel.testShow}, Cmd.none)
                _ -> (model, Cmd.none)
        (TestList, _) ->
            let (token, _) = fromModelTest model in (model, getListFiles FSNone TestRepList token)
        (TestCreate, _) ->
            let
                testObject = JE.object
                    [ ("key1", JE.string "value1")
                    , ("key2", JE.string "value2")
                    ]
                (token, name) = fromModelTest model
            in
            (model, createFile (FSCreate name testObject) TestRepString token)
        (TestRead, _) ->
            let (token, fileId) = fromModelTest model in
            (model, readFile (FSRead fileId) TestRepString decodeTestRead token)
        (TestDelete, _) ->
            let (token, fileId) = fromModelTest model in
            (model, deleteFile fileId TestRepUnit token)
        (TestRepList rep, _) -> (model, Cmd.none)
        (TestRepString rep, _) -> (model, Cmd.none)
        (TestRepValue rep, _) -> (model, Cmd.none)
        (TestRepUnit rep, _) -> (model, Cmd.none)
        (TestChange value, _) ->
            case model of
                Guest data -> (Guest {data | testValue = value}, Cmd.none)
                Home data -> (Home {data | testValue = value}, Cmd.none)
                Project data -> (Project {data | testValue = value}, Cmd.none)
        (_, _) -> (model, Cmd.none)

-- PORTS

port testCom : (String -> msg) -> Sub msg

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
    let genModel = getGenericModel model in
    Sub.batch
        [ testCom TestCom
        ,
            if genModel.apiToken.expiresIn > 0 then
                Time.every
                    (toFloat ((genModel.apiToken.expiresIn - 10) * 1000))
                    AskRefresh
            else Sub.none
        ]

-- VIEW

view : Model -> Browser.Document Msg
view model =
    let
        genModel = getGenericModel model
        getWaiter infos = div [class "waiter"] [text infos]
    in
    { title = "Tracy"
    , body =
        (case model of
                Guest guest ->
                    case guest.phase of
                        Checking -> getWaiter "Démarrage ..."
                        Panic ->
                            div [class "core error"]
                                [ p [] [text "Une erreur est survenue lors de la lecture de la configuration."]
                                , p [] [text "Un simple redémarrage peut parfois corriger le problème, mais dans le doute, contacter l'admin."]
                                ]
                        Login -> div [class "panel_connect"] [button [onClick AskLogIn, class "button"] [text "Connexion"]]
                        Refreshing -> getWaiter "Authorisation Drive ..."
                        Saving -> getWaiter "Sauvegarde configuration ..."
                        DriveOff error ->
                            div [class "core error"]
                                [ p [] [text "Il y a eu une erreur lors de la récupération des infos du drive :"]
                                , p [] [text error]
                                ]
                        Connecting -> getWaiter "Connexion en cours ..."
                        Failed error ->
                            div [class "core error"]
                                [ p [] [text "Vous êtes bien connecté, mais il y a eu une erreur lors du retour."]
                                , p [] [text "L'erreur remontée est la suivante :"]
                                , div [] [text error]
                                ]
                Home home -> Html.map HomeMsg (Home.view home)
                Project project -> Html.map ProjectMsg (Project.view project)
        )::(Html.viewIfLazy genModel.testShow (viewTest genModel))::[]
    }

-- TEST FUNCTIONS

decodeTest : Decoder (Array String)
decodeTest =
    field "files" (array string)

decodeTestRead : Decoder String
decodeTestRead =
    JD.oneOf [ field "projects" string, field "name" string]

fromModelTest : Model -> (Token, String)
fromModelTest model =
    case model of
        Guest data -> (data.apiToken.accessToken, data.testValue)
        Home data -> (data.apiToken.accessToken, data.testValue)
        Project data -> (data.apiToken.accessToken, data.testValue)

viewTest : GenericModel -> () -> Html Msg
viewTest genModel _ =
    div [class "zone_test"] [
        button [onClick TestList] [text "List"]
        , button [onClick TestRead] [text "Read"]
        , button [onClick TestCreate] [text "Create"]
        , input [value genModel.testValue, onInput TestChange, style "width" "250px"] []
        , button [onClick TestDelete] [text "Delete"]
        ]
