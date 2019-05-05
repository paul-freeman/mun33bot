module Google.Spreadsheets.Sheet exposing (Sheet, decode)

import Google.Spreadsheets.Sheets.GridData as GridData exposing (GridData)
import Google.Spreadsheets.Sheets.SheetProperties as SheetProperties exposing (SheetProperties)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E exposing (Value)


type alias Sheet =
    { properties : SheetProperties
    , data : Maybe (List GridData)
    , merges : Maybe (List GridRange)
    , conditionalFormats : Maybe (List ConditionalFormatRule)
    , filterViews : Maybe (List FilterView)
    , protectedRanges : Maybe (List ProtectedRange)
    , basicFilter : Maybe BasicFilter
    , charts : Maybe (List EmbeddedChart)
    , bandedRanges : Maybe (List BandedRange)
    , developerMetadata : Maybe (List DeveloperMetadata)
    , rowGroups : Maybe (List DimensionGroup)
    , columnGroups : Maybe (List DimensionGroup)
    }


decode : Decoder Sheet
decode =
    D.succeed Sheet
        |> required "properties" SheetProperties.decode
        |> optional "data" (D.map Just <| D.list GridData.decode) Nothing
        |> optional "merges" (D.map Just <| D.list decodeGridRange) Nothing
        |> optional "conditionalFormats" (D.map Just <| D.list decodeConditionalFormatRule) Nothing
        |> optional "filterViews" (D.map Just <| D.list decodeFilterView) Nothing
        |> optional "protectedRanges" (D.map Just <| D.list decodeProtectedRange) Nothing
        |> optional "basicFilter" (D.map Just decodeBasicFilter) Nothing
        |> optional "charts" (D.map Just <| D.list decodeEmbeddedChart) Nothing
        |> optional "bandedRanges" (D.map Just <| D.list decodeBandedRange) Nothing
        |> optional "developerMetadata" (D.map Just <| D.list decodeDeveloperMetadata) Nothing
        |> optional "rowGroups" (D.map Just <| D.list decodeDimensionGroup) Nothing
        |> optional "columnGroups" (D.map Just <| D.list decodeDimensionGroup) Nothing


type alias GridRange =
    Value


type alias ConditionalFormatRule =
    Value


type alias FilterView =
    Value


type alias ProtectedRange =
    Value


type alias BasicFilter =
    Value


type alias EmbeddedChart =
    Value


type alias BandedRange =
    Value


type alias DeveloperMetadata =
    Value


type alias DimensionGroup =
    Value


decodeGridRange : Decoder GridRange
decodeGridRange =
    D.succeed E.null


decodeConditionalFormatRule : Decoder ConditionalFormatRule
decodeConditionalFormatRule =
    D.succeed E.null


decodeFilterView : Decoder FilterView
decodeFilterView =
    D.succeed E.null


decodeProtectedRange : Decoder ProtectedRange
decodeProtectedRange =
    D.succeed E.null


decodeBasicFilter : Decoder BasicFilter
decodeBasicFilter =
    D.succeed E.null


decodeEmbeddedChart : Decoder EmbeddedChart
decodeEmbeddedChart =
    D.succeed E.null


decodeBandedRange : Decoder BandedRange
decodeBandedRange =
    D.succeed E.null


decodeDeveloperMetadata : Decoder DeveloperMetadata
decodeDeveloperMetadata =
    D.succeed E.null


decodeDimensionGroup : Decoder DimensionGroup
decodeDimensionGroup =
    D.succeed E.null
