port module Google.Spreadsheets exposing (Request(..), Response(..), request, response)

import Google.Client as Client
import Google.Spreadsheet as Spreadsheet exposing (Spreadsheet)
import Json.Decode as D
import Json.Encode as E


type Request
    = CreateSpreadsheet { name : String }
    | GetSpreadsheet { spreadsheetId : String }
    | UpdateSpreadsheet { spreadsheetId : String }


type Response
    = CreatedSpreadsheet { spreadsheetId : String }
    | GotSpreadsheet Spreadsheet
    | UpdatedSpreadsheet { spreadsheetId : String }


request : Request -> Cmd msg
request r =
    gapiClientSheetsSpreadsheetsRequest <|
        case r of
            CreateSpreadsheet { name } ->
                { method = "create"
                , args =
                    [ E.object []
                    , Spreadsheet.new name
                    ]
                }

            GetSpreadsheet { spreadsheetId } ->
                { method = "get"
                , args =
                    [ E.object
                        [ ( "spreadsheetId", E.string spreadsheetId )
                        ]
                    ]
                }

            UpdateSpreadsheet { spreadsheetId } ->
                { method = "batchUpdate"
                , args =
                    [ E.object
                        [ ( "spreadsheetId", E.string spreadsheetId )
                        ]
                    , E.object
                        [ ( "requests"
                          , E.list identity
                                [ E.object
                                    [ ( "addSheet"
                                      , E.object
                                            [ ( "properties"
                                              , E.object
                                                    [ ( "sheetId", E.int 1001 )
                                                    , ( "title", E.string "accounts" )
                                                    , ( "gridProperties"
                                                      , E.object
                                                            [ ( "columnCount", E.int 2 )
                                                            , ( "rowCount", E.int 10 )
                                                            ]
                                                      )
                                                    ]
                                              )
                                            ]
                                      )
                                    ]
                                , E.object
                                    [ ( "deleteSheet"
                                      , E.object
                                            [ ( "sheetId", E.int 0 )
                                            ]
                                      )
                                    ]
                                ]
                          )
                        ]
                    ]
                }


response : (Result String Response -> msg) -> Sub msg
response msg =
    gapiClientSheetsSpreadsheetsResponse
        (\res ->
            let
                decodeResult =
                    D.field "status" D.int
                        |> D.andThen
                            (\status ->
                                if status /= 200 then
                                    D.fail "response \"status\" not equal to 200"

                                else
                                    D.field "result" D.value
                            )
            in
            case res.method of
                "batchUpdate" ->
                    let
                        spreadsheetIdResult =
                            List.head res.args
                                |> Result.fromMaybe "missing head of args"
                                |> Result.andThen
                                    (D.decodeValue (D.field "spreadsheetId" D.string)
                                        >> Result.mapError D.errorToString
                                    )
                    in
                    case ( D.decodeValue decodeResult res.response, spreadsheetIdResult ) of
                        ( Ok _, Ok spreadsheetId ) ->
                            msg <| Ok <| UpdatedSpreadsheet { spreadsheetId = spreadsheetId }

                        ( Err error, _ ) ->
                            msg <| Err <| D.errorToString error

                        ( _, Err errorStr ) ->
                            msg <| Err errorStr

                "create" ->
                    case D.decodeValue (D.at [ "result", "spreadsheetId" ] D.string) res.response of
                        Ok spreadsheetId ->
                            msg <| Ok <| CreatedSpreadsheet { spreadsheetId = spreadsheetId }

                        Err error ->
                            msg <| Err <| D.errorToString error

                "get" ->
                    let
                        result =
                            D.decodeValue
                                (D.field "status" D.int
                                    |> D.andThen
                                        (\status ->
                                            if status /= 200 then
                                                D.fail "Google Sheets just sent me confusing information and I can't do anything with it."

                                            else
                                                D.field "result" Spreadsheet.decode
                                        )
                                )
                                res.response
                    in
                    case result of
                        Ok spreadsheet ->
                            msg <| Ok <| GotSpreadsheet spreadsheet

                        Err error ->
                            msg <| Err <| D.errorToString error

                _ ->
                    msg <| Err "unknown method in response"
        )


port gapiClientSheetsSpreadsheetsRequest : Client.Request -> Cmd msg


port gapiClientSheetsSpreadsheetsResponse : (Client.Response -> msg) -> Sub msg
