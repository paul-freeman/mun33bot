port module Google.Spreadsheets.Values exposing (gapiClientSheetsSpreadsheetsValuesRequest, gapiClientSheetsSpreadsheetsValuesResponse)

import Google.Client as Client


type Request
    = Noop


port gapiClientSheetsSpreadsheetsValuesRequest : Client.Request -> Cmd msg


port gapiClientSheetsSpreadsheetsValuesResponse : (Client.Response -> msg) -> Sub msg
