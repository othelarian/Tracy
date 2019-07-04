module Api exposing (ApiKey, FileId, FileSelector, ApiAction, Token, httpErrorToString, api??? makeRequest)

import Http
import Bytes.Encode as BE
import Json.Decode as JD
import Json.Decode exposing (Decoder, decodeValue, errorToString, field, int, list, string)
import Json.Encode as JE
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
    | DeleteFile

type alias FileId = String

type alias ApiKey = String

type alias Token = String

type FileSelector
    = FSNone
    --
    | FSCreate String String -- TODO : il faut ajouter la structure de données
    --
    | FSRead FileId String -- TODO : il faut ajouter une structure de capture, pour comprendre comment décoder les infos
    --
    | FSUpdate FileId String -- TODO : changer la string par une structure de données
    --
    | FSDelete FileId

-- PREPARE

prepareMetadata : FileSelector -> String
prepareMetadata selector =
    --
    -- TODO : il va falloir rendre cette structure générique
    --
    let
        base = "{'parents':['appDataFolder'],'mimeType':'application/json','name':'"
    in
    case selector of
        FSCreate name _ -> base++name++".json'}"
        _ -> ""

prepareMedia : FileSelector -> String
prepareMedia selector =
    --
    -- TODO : cette fonction n'est pas générique, il va falloir changer tout ça
    --
    ""
    --
    {-
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
    -}

prepareBytes : String -> (FileSelector -> String) -> FileSelector -> Http.Part
prepareBytes name fun value =
    fun value |> BE.string |> BE.encode |> Http.bytesPart name "application/json"

-- JSON DECODE

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

-- TODO : decodeReadFile ? ou alors il est fourni à travers l'api ?

-- TODO : pas besoin de decoder pour le delete, mais pour l'update ?

-- API REQUEST

apiGetListFiles : FileSelector -> (Result Http.Error (List InfoFile) -> msg) -> Token -> ApiKey -> Cmd msg
apiGetListFiles selector message token apiKey =
    makeRequest ListFiles selector message decodeListFiles token apiKey

apiCreateFile : FileSelector -> (Result Http.Error String -> msg) -> Token -> ApiKey -> Cmd msg
apiCreateFile selector message token apiKey =
    makeRequest CreateFile selector message decodeCreateFile token apiKey

--apiReadFile : FileSelector -> (Result Http.Error String -> msg) -> Token -> ApiKey -> Cmd msg
--apiReadFile selector message token apiKey =
--    makeRequest ReadFile selector message decodeReadFile token apiKey

--apiUpdateFile : FileSelector -> 

apiDeleteFile : FileSelector -> (Result Http.Error JD.Value -> msg) -> Token -> ApiKey -> Cmd msg
apiDeleteFile selector message token apiKey =
    makeRequest DeleteFile selector message JD.value token apiKey

-- MAIN REQUEST

makeRequest : ApiAction -> FileSelector -> (Result Http.Error a -> msg) -> (Decoder a) -> Token -> ApiKey -> Cmd msg
makeRequest action selector message decoder token apiKey =
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
        , expect = Http.expectJson message decoder
        , timeout = Nothing
        , tracker = Nothing
        }
