port module Google.Spreadsheets.Values exposing (Request(..), Response(..), receive, send)

import Google.Client as Client
import Google.Spreadsheets.Values.ValueRange as ValueRange exposing (ValueRange)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E exposing (Value)


type Request
    = GetAccounts { spreadsheetId : String }


send : Request -> Cmd msg
send request =
    gapiClientSheetsSpreadsheetsValuesRequest <|
        case request of
            GetAccounts { spreadsheetId } ->
                { method = "get"
                , args =
                    [ E.object
                        [ ( "spreadsheetId", E.string spreadsheetId )
                        , ( "range", E.string accountsRange )
                        ]
                    ]
                }


type Response
    = GotAccounts ValueRange


receive : (Result String Response -> msg) -> Sub msg
receive msg =
    gapiClientSheetsSpreadsheetsValuesResponse
        (\response ->
            let
                resultValue =
                    D.decodeValue
                        (D.field "status" D.int
                            |> D.andThen
                                (\status ->
                                    if status /= 200 then
                                        D.fail "Google Sheets just sent me confusing information and I can't do anything with it."

                                    else
                                        D.field "result" D.value
                                )
                        )
                        response.response
            in
            case response.method of
                "get" ->
                    let
                        mRanges =
                            List.head response.args
                                |> Maybe.map
                                    (D.decodeValue
                                        (D.field "range" D.string)
                                    )
                    in
                    if mRanges == Just (Ok accountsRange) then
                        case
                            resultValue
                                |> Result.andThen
                                    (D.decodeValue ValueRange.decode)
                        of
                            Ok values ->
                                msg <| Ok <| GotAccounts values

                            Err error ->
                                msg <| Err <| D.errorToString error

                    else
                        msg <| Err "unknown args in response"

                _ ->
                    msg <| Err "unknown method in response"
        )


accountsRange : String
accountsRange =
    "accounts!A1:B10"


port gapiClientSheetsSpreadsheetsValuesRequest : Client.Request -> Cmd msg


port gapiClientSheetsSpreadsheetsValuesResponse : (Client.Response -> msg) -> Sub msg
