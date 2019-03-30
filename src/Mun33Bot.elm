port module Mun33Bot exposing (main)

import Browser exposing (application)
import Browser.Navigation exposing (Key)
import Css exposing (..)
import Google.Spreadsheet exposing (..)
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
    , spreadsheet : Maybe Value
    , missingDataFile : Bool
    }


type Msg
    = Noop
    | ChangeSigninStatus Bool
    | HandleAuthClick
    | HandleSignoutClick
    | CreateNewGoogleSheet
    | RequestDriveFileList
    | GapiClientDriveFileResponse ClientResponse
    | GapiClientSheetsSpreadsheetsResponse ClientResponse
    | LogMsg Log.Msg


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


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( { signinStatus = Nothing
      , log = Log.default
      , spreadsheet = Nothing
      , missingDataFile = False
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
                                        , spreadsheet = Nothing
                                    }

        HandleAuthClick ->
            ( model
            , gapiAuth2GetAuthInstance
                { method = "signIn"
                , args = []
                }
            )

        HandleSignoutClick ->
            ( model
            , gapiAuth2GetAuthInstance
                { method = "signOut"
                , args = []
                }
            )

        CreateNewGoogleSheet ->
            ( model
            , gapiClientSheetsSpreadsheetsRequest
                { method = "create"
                , args =
                    [ E.object []
                    , Google.Spreadsheet.newSpreadsheet appData
                    ]
                }
            )

        RequestDriveFileList ->
            let
                ( newModel, newCmd ) =
                    update
                        (LogMsg <|
                            Log.ScheduleAddToLog
                                Log.Info
                                "I am asking Google Sheets for your data file now."
                        )
                        model
            in
            ( newModel
            , Cmd.batch
                [ newCmd
                , gapiClientDriveFileRequest
                    { method = "list"
                    , args =
                        [ E.object
                            [ ( "q", E.string <| ("name = '" ++ appData ++ "' and trashed = false") )
                            ]
                        ]
                    }
                ]
            )

        GapiClientDriveFileResponse { method, args, response } ->
            case method of
                "list" ->
                    let
                        decodeList =
                            D.field "files" <| D.list <| D.field "id" D.string
                    in
                    case D.decodeValue decodeList response of
                        Ok fileList ->
                            case List.head fileList of
                                Nothing ->
                                    update
                                        (LogMsg <|
                                            Log.ScheduleAddToLog
                                                Log.Info
                                                "I could not find a Google Sheet with your data. You can make a new one if you like."
                                        )
                                        { model | missingDataFile = True }

                                Just fileId ->
                                    ( { model | missingDataFile = False }
                                    , gapiClientSheetsSpreadsheetsRequest
                                        { method = "get"
                                        , args =
                                            [ E.object
                                                [ ( "spreadsheetId", E.string fileId )
                                                ]
                                            ]
                                        }
                                    )

                        Err error ->
                            update
                                (LogMsg <|
                                    Log.ScheduleAddToLog Log.Error <|
                                        D.errorToString error
                                )
                                model

                _ ->
                    update
                        (LogMsg <|
                            Log.ScheduleAddToLog Log.Info
                                "unknown GapiClientDriveFileResponse"
                        )
                        model

        GapiClientSheetsSpreadsheetsResponse { method, args, response } ->
            case method of
                "create" ->
                    let
                        result =
                            D.decodeValue
                                (D.field "status" D.int
                                    |> D.andThen
                                        (\status ->
                                            if status /= 200 then
                                                D.fail "Google Sheets just sent me confusing information and I can't do anything with it."

                                            else
                                                (D.field "result" <| D.field "spreadsheetId" D.string)
                                                    |> D.andThen (\fileId -> D.succeed fileId)
                                        )
                                )
                                response
                    in
                    case result of
                        Ok fileId ->
                            let
                                ( newModel, newCmd ) =
                                    update
                                        (LogMsg <|
                                            Log.ScheduleAddToLog
                                                Log.Info
                                                "I have created a new Google Sheet into which I will store your data. I will try to load it now."
                                        )
                                        model
                            in
                            ( newModel
                            , Cmd.batch
                                [ newCmd
                                , gapiClientSheetsSpreadsheetsRequest
                                    { method = "get"
                                    , args =
                                        [ E.object
                                            [ ( "spreadsheetId", E.string fileId )
                                            ]
                                        ]
                                    }
                                ]
                            )

                        Err error ->
                            update
                                (LogMsg <|
                                    Log.ScheduleAddToLog
                                        Log.Error
                                    <|
                                        D.errorToString
                                            error
                                )
                                model

                "get" ->
                    let
                        result =
                            D.decodeValue
                                (D.field "status" D.int
                                    |> D.andThen
                                        (\status ->
                                            if status /= 200 then
                                                D.fail "Google Sheets just sent me confusing information and I can't do anything with it."

                                            else
                                                (D.field "result" <| D.field "spreadsheetId" D.string)
                                                    |> D.andThen (\fileId -> D.succeed fileId)
                                        )
                                )
                                response
                    in
                    case result of
                        Ok fileId ->
                            update
                                (LogMsg <|
                                    Log.ScheduleAddToLog
                                        Log.Info
                                        "I have received your data from Google Sheets."
                                )
                                { model | spreadsheet = Just response, missingDataFile = False }

                        Err error ->
                            update
                                (LogMsg <|
                                    Log.ScheduleAddToLog
                                        Log.Error
                                        "Google Sheets just sent me confusing information and I can't do anything with it."
                                )
                                model

                _ ->
                    ( model, Cmd.none )

        LogMsg logMsg ->
            let
                ( logModel, logCmd ) =
                    Log.update logMsg model.log
            in
            ( { model | log = logModel }, Cmd.map LogMsg logCmd )


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
                        [ button [ onClick HandleAuthClick ] [ text "Authorize" ] ]

                    Just True ->
                        [ button [ onClick HandleSignoutClick ] [ text "Sign Out" ]
                        , div [] <|
                            case model.spreadsheet of
                                Just _ ->
                                    []

                                Nothing ->
                                    if model.missingDataFile then
                                        [ button [ onClick CreateNewGoogleSheet ] [ text "Create New Google Sheet" ] ]

                                    else
                                        [ button [ onClick RequestDriveFileList ] [ text "Load Google Sheet" ] ]
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
            [ gapiClientDriveFilesResponse GapiClientDriveFileResponse
            , gapiClientSheetsSpreadsheetsResponse GapiClientSheetsSpreadsheetsResponse
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


type alias ClientRequest =
    { method : String
    , args : List Value
    }


type alias ClientResponse =
    { method : String
    , args : List Value
    , response : Value
    }



-- COMMAND PORTS


port handleClientLoad : () -> Cmd msg


port gapiAuth2GetAuthInstance : ClientRequest -> Cmd msg


port gapiClientDriveFileRequest : ClientRequest -> Cmd msg


port gapiClientSheetsSpreadsheetsRequest : ClientRequest -> Cmd msg



-- SUBSCRIPTION PORTS


port updatedSigninStatus : (Bool -> msg) -> Sub msg


port gapiClientDriveFilesResponse : (ClientResponse -> msg) -> Sub msg


port gapiClientSheetsSpreadsheetsResponse : (ClientResponse -> msg) -> Sub msg


appData : String
appData =
    "mun33bot_appData_v0.1.0"
