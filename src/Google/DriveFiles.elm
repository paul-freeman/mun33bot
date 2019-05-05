port module Google.DriveFiles exposing (Request(..), Response(..), request, response)

import Google.Client as Client
import Json.Decode as D
import Json.Encode as E


type Request
    = RequestFileIdByName { name : String }


type Response
    = FileIdResponse (Maybe { fileId : String })


request : Request -> Cmd msg
request r =
    gapiClientDriveFileRequest <|
        case r of
            RequestFileIdByName { name } ->
                { method = "list"
                , args =
                    [ E.object
                        [ ( "q"
                          , [ "name = '"
                            , name
                            , "' and trashed = false"
                            ]
                                |> String.concat
                                |> E.string
                          )
                        ]
                    ]
                }


response : (Result String Response -> msg) -> Sub msg
response msg =
    gapiClientDriveFilesResponse
        (\res ->
            case res.method of
                "list" ->
                    let
                        decodeList =
                            D.field "files" <| D.list <| D.field "id" D.string
                    in
                    case D.decodeValue decodeList res.response of
                        Ok fileList ->
                            case List.head fileList of
                                Nothing ->
                                    msg (Ok <| FileIdResponse Nothing)

                                Just fileId ->
                                    msg (Ok <| FileIdResponse <| Just { fileId = fileId })

                        Err error ->
                            msg (Err <| D.errorToString error)

                _ ->
                    msg <| Err "unknown method in response"
        )


port gapiClientDriveFileRequest : Client.Request -> Cmd msg


port gapiClientDriveFilesResponse : (Client.Response -> msg) -> Sub msg
