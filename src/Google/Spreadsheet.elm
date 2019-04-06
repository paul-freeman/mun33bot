module Google.Spreadsheet exposing (Spreadsheet, decodeSpreadsheet, newSpreadsheet)

import Google.Spreadsheet.Sheet exposing (Sheet)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E exposing (Value)


type alias Spreadsheet =
    { spreadsheetId : String
    , properties : SpreadsheetProperties
    , sheets : List Sheet
    , namedRanges : List NamedRange
    , spreadsheetUrl : String
    , developerMetadata : List DeveloperMetadata
    }


type alias SpreadsheetProperties =
    { title : String
    , locale : String
    , autoRecalc : RecalculationInterval
    , timeZone : String
    , defaultFormat : CellFormat
    , iterativeCalculationSettings : Maybe IterativeCalculationSettings
    }


type alias NamedRange =
    Value


type alias DeveloperMetadata =
    Value


type alias RecalculationInterval =
    Value


type alias CellFormat =
    Value


type alias IterativeCalculationSettings =
    Value


newSpreadsheet : String -> Value
newSpreadsheet title =
    E.object
        [ ( "properties"
          , E.object
                [ ( "title", E.string title )
                ]
          )
        ]


decodeSpreadsheet : Decoder Spreadsheet
decodeSpreadsheet =
    D.succeed Spreadsheet
        |> required "spreadsheetId" D.string
        |> required "properties" decodeSpreadsheetProperties
        |> required "sheets" (D.list Google.Spreadsheet.Sheet.decodeSheet)
        |> optional "namedRanges" (D.list decodeNamedRange) []
        |> required "spreadsheetUrl" D.string
        |> optional "developerMetadata" (D.list decodeDeveloperMetadata) []


decodeSpreadsheetProperties : Decoder SpreadsheetProperties
decodeSpreadsheetProperties =
    D.succeed SpreadsheetProperties
        |> required "title" D.string
        |> required "locale" D.string
        |> required "autoRecalc" decodeRecalculationInterval
        |> required "timeZone" D.string
        |> required "defaultFormat" decodeCellFormat
        |> optional "iterativeCalculationSettings" (D.map Just decodeIterativeCalculationSettings) Nothing


decodeNamedRange : Decoder NamedRange
decodeNamedRange =
    D.succeed E.null


decodeDeveloperMetadata : Decoder DeveloperMetadata
decodeDeveloperMetadata =
    D.succeed E.null


decodeRecalculationInterval : Decoder RecalculationInterval
decodeRecalculationInterval =
    D.succeed E.null


decodeCellFormat : Decoder CellFormat
decodeCellFormat =
    D.succeed E.null


decodeIterativeCalculationSettings : Decoder IterativeCalculationSettings
decodeIterativeCalculationSettings =
    D.succeed E.null
