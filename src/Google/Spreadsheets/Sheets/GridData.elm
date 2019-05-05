module Google.Spreadsheets.Sheets.GridData exposing (GridData, decode)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E exposing (Value)


type alias GridData =
    { startRow : Int
    , startColumn : Int
    , rowData : List RowData
    , rowMetadata : List DimensionProperties
    , columnMetadata : List DimensionProperties
    }


decode : Decoder GridData
decode =
    D.succeed GridData
        |> required "startRow" D.int
        |> required "startColumn" D.int
        |> required "rowData" (D.list D.value)
        |> required "rowMetadata" (D.list D.value)
        |> required "columnMetadata" (D.list D.value)


type alias RowData =
    Value


type alias DimensionProperties =
    Value
