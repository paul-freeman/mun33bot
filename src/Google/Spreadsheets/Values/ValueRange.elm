module Google.Spreadsheets.Values.ValueRange exposing (ValueRange, decode)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E exposing (Value)


type alias ValueRange =
    { range : String
    , majorDimension : Dimension
    , values : Maybe (List Value)
    }


decode : Decoder ValueRange
decode =
    D.succeed ValueRange
        |> required "range" D.string
        |> required "majorDimension" decodeDimension
        |> optional "values" (D.map Just <| D.list D.value) Nothing


type Dimension
    = DimensionUnspecified -- do not use
    | Rows
    | Columns


decodeDimension : Decoder Dimension
decodeDimension =
    D.string
        |> D.andThen
            (\dimension ->
                case dimension of
                    "DIMENSIONS_UNSPECIFIED" ->
                        D.succeed DimensionUnspecified

                    "ROWS" ->
                        D.succeed Rows

                    "COLUMNS" ->
                        D.succeed Columns

                    _ ->
                        D.fail <| "Unknown Dimension value: " ++ dimension
            )
