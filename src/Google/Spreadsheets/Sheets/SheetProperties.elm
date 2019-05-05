module Google.Spreadsheets.Sheets.SheetProperties exposing (SheetProperties, decode)

import Google.Spreadsheets.Sheets.GridProperties as GridProperties exposing (GridProperties, decode)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E exposing (Value)


type alias SheetProperties =
    { sheetId : Int
    , title : String
    , index : Int
    , sheetType : SheetType
    , gridProperties : GridProperties
    , hidden : Maybe Bool
    , tabColor : Maybe Color
    , rightToLeft : Maybe Bool
    }


decode : Decoder SheetProperties
decode =
    D.succeed SheetProperties
        |> required "sheetId" D.int
        |> required "title" D.string
        |> required "index" D.int
        |> required "sheetType" decodeSheetType
        |> required "gridProperties" GridProperties.decode
        |> optional "hidden" (D.map Just D.bool) Nothing
        |> optional "tabColor" (D.map Just decodeColor) Nothing
        |> optional "rightToLeft" (D.map Just D.bool) Nothing


type alias SheetType =
    Value


decodeSheetType : Decoder SheetType
decodeSheetType =
    D.succeed E.null


type alias Color =
    Value


decodeColor : Decoder Color
decodeColor =
    D.succeed E.null
