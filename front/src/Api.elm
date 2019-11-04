module Api exposing
    ( ApiToken
    , ApiAction(..)
    , FileId
    , FileSelector(..)
    , InfoFile
    , Token
    , createFile
    , createIdentityUrl
    , decodeInfoFile
    , deleteFile
    , getFirstAccess
    , getListFiles
    , getRefreshAccess
    , httpErrorToString
    , readFile
    , updateFile
    )

import Keys

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

type alias ApiToken =
    { refreshToken : String
    , accessToken : String
    , expiresIn : Int
    }

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

type alias Token = String

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

decodeRefreshToken : Decoder ApiToken
decodeRefreshToken =
    JD.map3 ApiToken
        (field "refresh_token" string)
        (field "access_token" string)
        (field "expires_in" int)

decodeAccessToken : String -> Decoder ApiToken
decodeAccessToken refreshToken =
    JD.map2 (ApiToken refreshToken) (field "access_token" string) (field "expires_in" int)

decodeInfoFile : Decoder InfoFile
decodeInfoFile =
    JD.map2 InfoFile (field "id" string) (field "name" string)

decodeListFiles : Decoder (List InfoFile)
decodeListFiles =
    field "files" (list decodeInfoFile)

decodeUploadFile : Decoder String
decodeUploadFile =
    field "id" string

-- API URL BUILDER

createIdentityUrl : Url.Url -> String
createIdentityUrl url =
    let newUrl = {url | path = "/app.html", query = Nothing, fragment = Nothing} in
    UB.crossOrigin
        "https://accounts.google.com"
        ["o", "oauth2", "v2", "auth"]
        [ UB.string "scope" Keys.getScopes
        , UB.string "access_type" "offline"
        , UB.string "state" "state_parameter_passthrough_value"
        , UB.string "redirect_uri" (Url.toString newUrl)
        , UB.string "response_type" "code"
        , UB.string "client_id" Keys.getClientId
        ]

-- API TOKEN REQUESTS

createTokenApiBody : List (String, String) -> String
createTokenApiBody args =
    let reduceTuple (first, second) = first++"="++second in
    String.join "&" (List.map reduceTuple args)

getFirstAccess : String -> Url.Url -> (Result Http.Error ApiToken -> msg) -> Cmd msg
getFirstAccess code url message =
    let newUrl = {url | path = "/app.html", query = Nothing, fragment = Nothing} in
    Http.post
        { url = UB.crossOrigin "https://www.googleapis.com" ["oauth2", "v4", "token"] []
        , body = Http.stringBody
            "application/x-www-form-urlencoded"
            (createTokenApiBody
                [ ("code", code)
                , ("client_id", Keys.getClientId)
                , ("client_secret", Keys.getClientSecret)
                , ("redirect_uri", (Url.toString newUrl))
                , ("grant_type", "authorization_code")
                ]
            )
        , expect = Http.expectJson message decodeRefreshToken
        }

getRefreshAccess : String -> (Result Http.Error ApiToken -> msg) -> Cmd msg
getRefreshAccess refreshToken message =
    Http.post
        { url = UB.crossOrigin "https://www.googleapis.com" ["oauth2", "v4", "token"] []
        , body = Http.stringBody
            "application/x-www-form-urlencoded"
            (createTokenApiBody
                [ ("client_id", Keys.getClientId)
                , ("client_secret", Keys.getClientSecret)
                , ("refresh_token", refreshToken)
                , ("grant_type", "refresh_token")
                ]
            )
        , expect = Http.expectJson message (decodeAccessToken refreshToken)
        }

-- API DRIVE REQUESTS

getListFiles : FileSelector -> (Result Http.Error (List InfoFile) -> msg) -> Token -> Cmd msg
getListFiles selector message token =
    makeRequest ListFiles selector message decodeListFiles token

createFile : FileSelector -> (Result Http.Error String -> msg) -> Token -> Cmd msg
createFile selector message token =
    makeRequest CreateFile selector message decodeUploadFile token

readFile : FileSelector -> (Result Http.Error a -> msg) -> (Decoder a) -> Token -> Cmd msg
readFile selector message decoder token =
    makeRequest ReadFile selector message decoder token

updateFile : FileSelector -> (Result Http.Error String -> msg) -> Token -> Cmd msg
updateFile selector message token =
    makeRequest UpdateFile selector message decodeUploadFile token

deleteFile : FileId -> (Result Http.Error () -> msg) -> Token -> Cmd msg
deleteFile fileId message token =
    Http.request
        { method = "DELETE"
        , headers = [(Http.header "authorization" ("Bearer "++token))]
        , url = (UB.crossOrigin "https://www.googleapis.com" ["drive", "v3", "files", fileId] [])
        , body = Http.emptyBody
        , expect = Http.expectWhatever message
        , timeout = Nothing
        , tracker = Nothing
        }

-- DRIVE REQUEST MAKER

makeRequest : ApiAction -> FileSelector -> (Result Http.Error a -> msg) -> (Decoder a) -> Token -> Cmd msg
makeRequest action selector message decoder token =
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
                (let key = [UB.string "key" Keys.getApiKey] in
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
