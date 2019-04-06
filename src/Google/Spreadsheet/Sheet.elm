module Google.Spreadsheet.Sheet exposing (Sheet, decodeSheet)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E exposing (Value)


type alias Sheet =
    { properties : SheetProperties
    , data : List GridData
    , merges : List GridRange
    , conditionalFormats : List ConditionalFormatRule
    , filterViews : List FilterView
    , protectedRanges : List ProtectedRange
    , basicFilter : Maybe BasicFilter
    , charts : List EmbeddedChart
    , bandedRanges : List BandedRange
    , developerMetadata : List DeveloperMetadata
    , rowGroups : List DimensionGroup
    , columnGroups : List DimensionGroup
    }


type alias SheetProperties =
    Value


type alias GridData =
    Value


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


decodeSheet : Decoder Sheet
decodeSheet =
    D.succeed Sheet
        |> required "properties" decodeSheetProperties
        |> optional "data" (D.list decodeGridData) []
        |> optional "merges" (D.list decodeGridRange) []
        |> optional "conditionalFormats" (D.list decodeConditionalFormatRule) []
        |> optional "filterViews" (D.list decodeFilterView) []
        |> optional "protectedRanges" (D.list decodeProtectedRange) []
        |> optional "basicFilter" (D.map Just decodeBasicFilter) Nothing
        |> optional "charts" (D.list decodeEmbeddedChart) []
        |> optional "bandedRanges" (D.list decodeBandedRange) []
        |> optional "developerMetadata" (D.list decodeDeveloperMetadata) []
        |> optional "rowGroups" (D.list decodeDimensionGroup) []
        |> optional "columnGroups" (D.list decodeDimensionGroup) []


decodeSheetProperties : Decoder SheetProperties
decodeSheetProperties =
    D.succeed E.null


decodeGridData : Decoder GridData
decodeGridData =
    D.succeed E.null


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
