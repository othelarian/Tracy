module Api exposing (FileId, FileSelector, HttpAction, httpErrorToString, makeRequest, makeRequestModel)

import Types exposing (..)

import Http
import Bytes.Encode as BE
import Url
import Url.Builder as UB

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

type alias FileId = String

type alias FileProject = Bool

type FileSelector
    = FSNone
    | FSCreate String FileProject
    | FSRead FileId FileProject
    | FSUpdate FileId String -- TODO : changer la string par une structure de données
    | FSDelete FileId

prepareMetadata : FileSelector -> String
prepareMetadata selector =
    let
        base = "{'parents':['appDataFolder'],'mimeType':'application/json','name':'"
    in
    case selector of
        FSCreate name _ -> base++name++".json'}"
        _ -> ""

prepareMedia : FileSelector -> String
prepareMedia selector =
    case selector of
        FSCreate _ isProject ->
            if isProject then
                "{'name':'new project'}"
            else
                "{'projects':[]}"
        --
        -- TODO : cas du update à traiter ici
        --
        _ -> ""

prepareBytes : String -> (FileSelector -> String) -> FileSelector -> Http.Part
prepareBytes name fun value =
    fun value |> BE.string |> BE.encode |> Http.bytesPart name "application/json"

makeRequestModel : HttpAction -> FileSelector -> Model -> Cmd msg
makeRequestModel action selector model =
    makeRequest action selector model.token model.api_key

makeRequest : HttpAction -> FileSelector -> Token -> ApiKey -> Cmd msg
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
                        FSRead id _ -> id
                        FSUpdate id _ -> id
                        FSDelete id -> id
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
                    CreateFile -> (UB.string "alt" "json")::(UB.string "uploadType" "multipart")::key
                    ReadFile -> (UB.string "alt" "media")::key
                    --
                    -- TODO : ajouter les élément manquant, si besoin
                    --
                    _ -> []
                )
            )
        , body =
            if action == CreateFile || action == UpdateFile then
                Http.multipartBody
                    [ prepareBytes "MetaData" prepareMetadata selector
                    , prepareBytes "Media"  prepareMedia selector
                    ]
            else
                Http.emptyBody
        , expect = case action of
            ListFiles -> Http.expectJson GetListFiles decodeListFiles
            CreateFile -> Http.expectJson GetCreateFile decodeCreateFile
            ReadFile ->
                let
                    isProject = case selector of
                        FSRead _ bool -> bool
                        _ -> False
                in
                case isProject of
                    False -> Http.expectJson GetReadHome decodeReadHome
                    True -> Http.expectJson GetReadProject decodeReadProject
            --
            -- TODO : gérer tout les cas
            --
            _ -> Http.expectJson TestRep decodeTest
            --
        , timeout = Nothing
        , tracker = Nothing
        }
