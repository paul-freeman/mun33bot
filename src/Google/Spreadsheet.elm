module Google.Spreadsheet exposing (Spreadsheet, newSpreadsheet)

import Json.Encode as E exposing (Value)


type Spreadsheet
    = Spreadsheet
        { spreadsheetId : String
        , properties : SpreadsheetProperties
        , sheets : List Sheet
        , namedRanges : List NamedRange
        , spreadsheetUrl : String
        , developerMetadata : List DeveloperMetadata
        }


type SpreadsheetProperties
    = SpreadsheetProperties
        { title : String
        , locale : String
        , autoRecalc : RecalculationInterval
        , timeZone : String
        , defaultFormat : CellFormat
        , iterativeCalculationSettings : IterativeCalculationSettings
        }


type alias Sheet =
    Value


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
