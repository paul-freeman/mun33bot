port module Mun33Bot exposing (main)

import Browser exposing (application)
import Browser.Navigation as Nav
import Css exposing (..)
import Google.Client as Client
import Google.DriveFiles as DriveFiles
import Google.Spreadsheet as Spreadsheet exposing (Spreadsheet)
import Google.Spreadsheets as Spreadsheets
import Google.Spreadsheets.Values as Values
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Http exposing (..)
import Json.Decode as D
import Json.Encode as E exposing (Value)
import Log
import Task
import Url exposing (Url)
import VirtualDom exposing (Node)


type alias Flags =
    ()


type alias Model =
    { signinStatus : Maybe Bool
    , log : Log.Model
    , spreadsheetId : SpreadsheetId
    , spreadsheet : Maybe Spreadsheet
    , browserKey : Nav.Key
    }


type SpreadsheetId
    = Unknown
    | Missing
    | Id String


type Msg
    = Noop
    | ChangeSigninStatus Bool
    | CreatedSpreadsheet { spreadsheetId : String }
    | FoundId (Maybe { fileId : String })
    | LoadedSpreadsheet Spreadsheet
    | LogMsg Log.Msg
    | RequestedNew
    | RequestedLoad
    | RunCmd (Cmd Msg)
    | UpdatedSpreadsheet { spreadsheetId : String }


main : Program Flags Model Msg
main =
    application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { signinStatus = Nothing
      , log = Log.default
      , spreadsheetId = Unknown
      , spreadsheet = Nothing
      , browserKey = key
      }
    , handleClientLoad ()
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        ChangeSigninStatus isSignedIn ->
            case model.signinStatus of
                Nothing ->
                    update
                        (if isSignedIn then
                            LogMsg <|
                                Log.ScheduleAddToLog
                                    Log.Info
                                    "Google has notified me that you are signed in."

                         else
                            LogMsg <|
                                Log.ScheduleAddToLog
                                    Log.Info
                                    "Google has notified me that you are not signed in."
                        )
                        { model | signinStatus = Just isSignedIn }

                Just signInStatus ->
                    if signInStatus == isSignedIn then
                        ( model, Cmd.none )

                    else
                        case isSignedIn of
                            True ->
                                update
                                    (LogMsg <|
                                        Log.ScheduleAddToLog
                                            Log.Info
                                            "You have been logged in."
                                    )
                                    { model | signinStatus = Just True }

                            False ->
                                update
                                    (LogMsg <|
                                        Log.ScheduleAddToLog
                                            Log.Info
                                            "You have been logged out."
                                    )
                                    { model
                                        | signinStatus = Just False
                                        , spreadsheetId = Unknown
                                    }

        RequestedNew ->
            ( model
            , Spreadsheets.request <| Spreadsheets.CreateSpreadsheet { name = appData }
            )

        RequestedLoad ->
            let
                ( newModel, newCmd ) =
                    update
                        ("I am asking Google Sheets for your data file now."
                            |> Log.ScheduleAddToLog Log.Info
                            |> LogMsg
                        )
                        model
            in
            ( newModel
            , Cmd.batch
                [ newCmd
                , DriveFiles.request <|
                    DriveFiles.RequestFileIdByName { name = appData }
                ]
            )

        FoundId maybeId ->
            case maybeId of
                Just { fileId } ->
                    ( { model | spreadsheetId = Id fileId }
                    , Spreadsheets.request <| Spreadsheets.GetSpreadsheet { spreadsheetId = fileId }
                    )

                Nothing ->
                    update
                        ([ "I could not find a Google Sheet "
                         , "with your data. You can make a "
                         , "new one if you like."
                         ]
                            |> String.concat
                            |> Log.ScheduleAddToLog Log.Info
                            |> LogMsg
                        )
                        { model | spreadsheetId = Missing }

        CreatedSpreadsheet { spreadsheetId } ->
            let
                ( newModel, newCmd ) =
                    update
                        ([ "I have created a new Google Sheet into "
                         , "which I will store your data. I will "
                         , "try to set it up for use."
                         ]
                            |> String.concat
                            |> Log.ScheduleAddToLog Log.Info
                            |> LogMsg
                        )
                        model
            in
            ( newModel
            , Cmd.batch
                [ newCmd
                , Spreadsheets.request <| Spreadsheets.UpdateSpreadsheet { spreadsheetId = spreadsheetId }
                ]
            )

        UpdatedSpreadsheet { spreadsheetId } ->
            update Noop model

        LoadedSpreadsheet spreadsheet ->
            update
                (LogMsg <|
                    Log.ScheduleAddToLog
                        Log.Info
                        "I have received your data from Google Sheets."
                )
                { model
                    | spreadsheetId = Id spreadsheet.spreadsheetId
                    , spreadsheet = Just spreadsheet
                }

        LogMsg logMsg ->
            let
                ( logModel, logCmd ) =
                    Log.update logMsg model.log
            in
            ( { model | log = logModel }, Cmd.map LogMsg logCmd )

        RunCmd cmd ->
            ( model, cmd )


view : Model -> { title : String, body : List (Node Msg) }
view model =
    { title = "Mun33Bot"
    , body =
        List.map toUnstyled
            [ p [] [ text "Drive API Quickstart" ]
            , div [] <|
                case model.signinStatus of
                    Nothing ->
                        [ p [] [ text "Waiting for Google to come online." ] ]

                    Just False ->
                        [ button [ onClick <| RunCmd Client.signIn ] [ text "Authorize" ] ]

                    Just True ->
                        [ button [ onClick <| RunCmd Client.signOut ] [ text "Sign Out" ]
                        , div [] <|
                            case model.spreadsheet of
                                Just _ ->
                                    []

                                Nothing ->
                                    if model.spreadsheetId == Missing then
                                        [ button [ onClick RequestedNew ] [ text "Create New Google Sheet" ] ]

                                    else
                                        [ button [ onClick RequestedLoad ] [ text "Load Google Sheet" ] ]
                        ]
            , Html.Styled.map LogMsg <| Log.view model.log
            ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    [ [ updatedSigninStatus ChangeSigninStatus ]
    , case model.signinStatus of
        Nothing ->
            []

        Just True ->
            [ DriveFiles.response
                (\res ->
                    case res of
                        Ok (DriveFiles.FileIdResponse maybeId) ->
                            FoundId maybeId

                        Err error ->
                            LogMsg <| Log.ScheduleAddToLog Log.Error error
                )
            , Spreadsheets.response
                (\res ->
                    case res of
                        Ok (Spreadsheets.CreatedSpreadsheet { spreadsheetId }) ->
                            CreatedSpreadsheet { spreadsheetId = spreadsheetId }

                        Ok (Spreadsheets.GotSpreadsheet spreadsheet) ->
                            LoadedSpreadsheet spreadsheet

                        Ok (Spreadsheets.UpdatedSpreadsheet { spreadsheetId }) ->
                            UpdatedSpreadsheet { spreadsheetId = spreadsheetId }

                        Err error ->
                            LogMsg <| Log.ScheduleAddToLog Log.Error error
                )
            , Values.gapiClientSheetsSpreadsheetsValuesResponse (always Noop)
            ]

        Just False ->
            []
    , [ Sub.map LogMsg <| Log.subscriptions model.log ]
    ]
        |> List.concat
        |> Sub.batch


onUrlRequest urlRequest =
    Noop


onUrlChange url =
    Noop



-- COMMAND PORTS


port handleClientLoad : () -> Cmd msg



-- SUBSCRIPTION PORTS


port updatedSigninStatus : (Bool -> msg) -> Sub msg


appData : String
appData =
    "mun33bot_appData_v0.1.0"
