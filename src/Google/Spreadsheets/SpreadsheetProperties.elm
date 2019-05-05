module Google.Spreadsheets.SpreadsheetProperties exposing (SpreadsheetProperties, decode)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E exposing (Value)


type alias SpreadsheetProperties =
    { title : String
    , locale : String
    , autoRecalc : RecalculationInterval
    , timeZone : String
    , defaultFormat : CellFormat
    , iterativeCalculationSettings : Maybe IterativeCalculationSettings
    }


decode : Decoder SpreadsheetProperties
decode =
    D.succeed SpreadsheetProperties
        |> required "title" D.string
        |> required "locale" D.string
        |> required "autoRecalc" decodeRecalculationInterval
        |> required "timeZone" D.string
        |> required "defaultFormat" decodeCellFormat
        |> optional "iterativeCalculationSettings" (D.map Just decodeIterativeCalculationSettings) Nothing


type alias RecalculationInterval =
    Value


type alias CellFormat =
    Value


type alias IterativeCalculationSettings =
    Value


decodeRecalculationInterval : Decoder RecalculationInterval
decodeRecalculationInterval =
    D.succeed E.null


decodeCellFormat : Decoder CellFormat
decodeCellFormat =
    D.succeed E.null


decodeIterativeCalculationSettings : Decoder IterativeCalculationSettings
decodeIterativeCalculationSettings =
    D.succeed E.null
