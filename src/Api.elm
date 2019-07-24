module Api exposing
    ( ApiAction(..)
    , ApiKey
    , ApiCredentials
    , InfoFile
    , FileId
    , FileSelector(..)
    , Token
    , httpErrorToString
    , apiGetListFiles
    , apiCreateFile
    , apiReadFile
    , apiUpdateFile
    , apiDeleteFile
    , decodeInfoFile
    )

import Bytes.Encode as BE
import Json.Decode as JD
import Json.Decode exposing (Decoder, decodeValue, errorToString, field, int, list, string)
import Json.Encode as JE
import Http
import Url
import Url.Builder as UB

-- PROCESSING ERROR

httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl badUrl -> ("Vous avez tentez d'appeler ça ? vraiment ? -> "++badUrl)
        Http.Timeout -> "Vous avez pris trop de temps ..."
        Http.NetworkError -> "Il y a un truc avec le réseau ..."
        Http.BadStatus status -> ("Il y a un soucis, voici le code : "++(String.fromInt status))
        Http.BadBody body -> ("Vous avez reçu un message étonnant :\n"++body)

-- API TYPES

type ApiAction
    = ListFiles
    | CreateFile
    | ReadFile
    | UpdateFile

type alias FileId = String

type alias InfoFile =
    { fileId : FileId
    , name : String
    }

type alias ApiKey = String

type alias Token = String

type alias ApiCredentials =
    { apiKey : ApiKey
    , accessToken : Token
    , refreshToken : Token
    }

type FileSelector
    = FSNone
    | FSCreate String JE.Value
    | FSRead FileId
    | FSUpdate FileId JE.Value

-- PREPARE

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
        FSCreate _ value -> JE.encode 0 value
        FSUpdate _ value -> JE.encode 0 value
        _ -> ""

prepareBytes : String -> (FileSelector -> String) -> FileSelector -> Http.Part
prepareBytes name fun value =
    fun value |> BE.string |> BE.encode |> Http.bytesPart name "application/json"

-- JSON DECODE

decodeInfoFile : Decoder InfoFile
decodeInfoFile =
    JD.map2 InfoFile (field "id" string) (field "name" string)

decodeListFiles : Decoder (List InfoFile)
decodeListFiles =
    field "files" (list decodeInfoFile)

decodeUploadFile : Decoder String
decodeUploadFile =
    field "id" string

-- API REQUEST

apiGetListFiles : FileSelector -> (Result Http.Error (List InfoFile) -> msg) -> ApiCredentials -> Cmd msg
apiGetListFiles selector message credentials =
    makeRequest ListFiles selector message decodeListFiles credentials.accessToken credentials.apiKey

apiCreateFile : FileSelector -> (Result Http.Error String -> msg) -> ApiCredentials -> Cmd msg
apiCreateFile selector message credentials =
    makeRequest CreateFile selector message decodeUploadFile credentials.accessToken credentials.apiKey

apiReadFile : FileSelector -> (Result Http.Error a -> msg) -> (Decoder a) -> ApiCredentials -> Cmd msg
apiReadFile selector message decoder credentials =
    makeRequest ReadFile selector message decoder credentials.accessToken credentials.apiKey

apiUpdateFile : FileSelector -> (Result Http.Error String -> msg) -> ApiCredentials -> Cmd msg
apiUpdateFile selector message credentials =
    makeRequest UpdateFile selector message decodeUploadFile credentials.accessToken credentials.apiKey

apiDeleteFile : FileId -> (Result Http.Error () -> msg) -> ApiCredentials -> Cmd msg
apiDeleteFile fileId message credentials =
    Http.request
        { method = "DELETE"
        , headers = [(Http.header "authorization" ("Bearer "++credentials.accessToken))]
        , url = (UB.crossOrigin "https://www.googleapis.com" ["drive", "v3", "files", fileId] [])
        , body = Http.emptyBody
        , expect = Http.expectWhatever message
        , timeout = Nothing
        , tracker = Nothing
        }

-- MAIN REQUEST

makeRequest : ApiAction -> FileSelector -> (Result Http.Error a -> msg) -> (Decoder a) -> Token -> ApiKey -> Cmd msg
makeRequest action selector message decoder token apiKey =
    Http.request
        { method = case action of
            CreateFile -> "POST"
            UpdateFile -> "PATCH"
            _ -> "GET"
        , headers = [(Http.header "authorization" ("Bearer "++token))]
        , url = (UB.crossOrigin
                "https://www.googleapis.com"
                (let
                    path = ["drive", "v3", "files"]
                    fileId = case selector of
                        FSRead id -> id
                        FSUpdate id _ -> id
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
                    UpdateFile -> (UB.string "alt" "json")::(UB.string "uploadType" "multipart")::key
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
        , expect = Http.expectJson message decoder
        , timeout = Nothing
        , tracker = Nothing
        }
