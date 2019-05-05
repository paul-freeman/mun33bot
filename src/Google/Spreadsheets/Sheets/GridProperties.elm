module Google.Spreadsheets.Sheets.GridProperties exposing (GridProperties, decode)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E exposing (Value)


type alias GridProperties =
    { rowCount : Int
    , columnCount : Int
    , frozenRowCount : Maybe Int
    , frozenColumnCount : Maybe Int
    , hideGridlines : Maybe Bool
    , rowGroupControlAfter : Maybe Bool
    , columnGroupControlAfter : Maybe Bool
    }


decode : Decoder GridProperties
decode =
    D.succeed GridProperties
        |> required "rowCount" D.int
        |> required "columnCount" D.int
        |> optional "frozenRowCount" (D.map Just <| D.int) Nothing
        |> optional "frozenColumnCount" (D.map Just <| D.int) Nothing
        |> optional "hideGridLines" (D.map Just <| D.bool) Nothing
        |> optional "rowGroupControlAfter" (D.map Just <| D.bool) Nothing
        |> optional "columnGroupControlAfter" (D.map Just <| D.bool) Nothing
