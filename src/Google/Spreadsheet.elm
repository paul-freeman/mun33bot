module Google.Spreadsheet exposing (Spreadsheet, decode, new)

import Google.Spreadsheets.Sheet as Sheet exposing (Sheet)
import Google.Spreadsheets.SpreadsheetProperties as SpreadsheetProperties exposing (SpreadsheetProperties)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E exposing (Value)


type alias Spreadsheet =
    { spreadsheetId : String
    , properties : SpreadsheetProperties
    , sheets : List Sheet
    , namedRanges : Maybe (List NamedRange)
    , spreadsheetUrl : String
    , developerMetadata : Maybe (List DeveloperMetadata)
    }


type alias NamedRange =
    Value


type alias DeveloperMetadata =
    Value


new : String -> Value
new title =
    E.object
        [ ( "properties"
          , E.object
                [ ( "title", E.string title )
                ]
          )
        ]


decode : Decoder Spreadsheet
decode =
    D.succeed Spreadsheet
        |> required "spreadsheetId" D.string
        |> required "properties" SpreadsheetProperties.decode
        |> required "sheets" (D.list Sheet.decode)
        |> optional "namedRanges" (D.map Just <| D.list D.value) Nothing
        |> required "spreadsheetUrl" D.string
        |> optional "developerMetadata" (D.map Just <| D.list D.value) Nothing
