port module Mun33Bot exposing (main)

import Browser exposing (application)
import Browser.Navigation as Nav
import Google.Client as Client
import Google.DriveFiles as DriveFiles
import Google.Spreadsheet as Spreadsheet exposing (Spreadsheet)
import Google.Spreadsheets as Spreadsheets
import Google.Spreadsheets.Values as Values
import Google.Spreadsheets.Values.ValueRange exposing (ValueRange)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as D
import Json.Encode as E exposing (Value)
import Log
import Task
import Url exposing (Url)
import VirtualDom exposing (Node)


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


type alias Model =
    { accounts : Maybe ValueRange
    , browserKey : Nav.Key
    , currentView : View
    , flags : Flags
    , signinStatus : Maybe Bool
    , spreadsheetId : SpreadsheetId
    , url : Url
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { accounts = Nothing
      , browserKey = key
      , currentView = MainView
      , flags = flags
      , signinStatus = Nothing
      , spreadsheetId = Unknown
      , url = url
      }
    , handleClientLoad ()
    )


type Msg
    = Noop
    | ChangeSigninStatus Bool
    | ChangeView View
    | CreatedSpreadsheet { spreadsheetId : String }
    | FoundDataFile (Maybe { fileId : String })
    | RunCmd (Cmd Msg)
    | UpdatedAccountData { accounts : ValueRange }
    | UpdatedSpreadsheet { spreadsheetId : String }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        ChangeSigninStatus isSignedIn ->
            if isSignedIn then
                ( { model | signinStatus = Just True }
                , DriveFiles.request <|
                    DriveFiles.RequestFileIdByName { name = appData }
                )

            else
                let
                    ( newModel, newCmd ) =
                        init model.flags model.url model.browserKey
                in
                ( { newModel | signinStatus = Just False }, newCmd )

        ChangeView newView ->
            ( { model | currentView = newView }, Cmd.none )

        CreatedSpreadsheet { spreadsheetId } ->
            ( model
            , Spreadsheets.request <|
                Spreadsheets.UpdateSpreadsheet { spreadsheetId = spreadsheetId }
            )

        FoundDataFile maybeId ->
            case maybeId of
                Just { fileId } ->
                    ( { model | spreadsheetId = Id fileId }
                    , Values.send <|
                        Values.GetAccounts
                            { spreadsheetId = fileId }
                    )

                Nothing ->
                    ( { model | spreadsheetId = Missing }
                    , Spreadsheets.request <|
                        Spreadsheets.CreateSpreadsheet { name = appData }
                    )

        RunCmd cmd ->
            ( model, cmd )

        UpdatedAccountData { accounts } ->
            ( { model | accounts = Just accounts }, Cmd.none )

        UpdatedSpreadsheet { spreadsheetId } ->
            ( { model | spreadsheetId = Id spreadsheetId }, Cmd.none )


type alias Flags =
    ()


type SpreadsheetId
    = Unknown
    | Missing
    | Id String


type View
    = MainView
    | AccountsView ValueRange


view : Model -> { title : String, body : List (Node Msg) }
view model =
    { title = "Mun33Bot"
    , body =
        [ section [ class "section" ]
            [ div [ class "container" ] <|
                case model.signinStatus of
                    Nothing ->
                        [ customButton
                            [ class "button is-primary is-outlined is-fullwidth is-loading"
                            , disabled True
                            ]
                            [ text "Mun33Bot is loading..." ]
                        ]

                    Just False ->
                        [ customButton
                            [ class "button is-primary is-outlined is-fullwidth"
                            , disabled True
                            ]
                            [ text "Please authorize this app to use Google..." ]
                        , customButton
                            [ class "button is-primary is-fullwidth"
                            , onClick <| RunCmd Client.signIn
                            ]
                            [ text "Authorize" ]
                        ]

                    Just True ->
                        case model.spreadsheetId of
                            Unknown ->
                                [ customButton
                                    [ class "button is-primary is-outlined is-fullwidth is-loading"
                                    , disabled True
                                    ]
                                    [ text "Loading data..." ]
                                , customButton
                                    [ class "button is-danger is-fullwidth"
                                    , onClick <| RunCmd Client.signOut
                                    ]
                                    [ text "Sign Out" ]
                                ]

                            Missing ->
                                [ customButton
                                    [ class "button is-danger is-outlined is-fullwidth"
                                    , disabled True
                                    ]
                                    [ text "Failed to load data" ]
                                , customButton
                                    [ class "button is-danger is-fullwidth"
                                    , onClick <| RunCmd Client.signOut
                                    ]
                                    [ text "Sign Out" ]
                                ]

                            Id spreadsheetId ->
                                case model.currentView of
                                    MainView ->
                                        [ mainView model ]

                                    AccountsView accounts ->
                                        [ Html.div [ class "Account-view" ] <|
                                            (case accounts.values of
                                                Nothing ->
                                                    [ customButton
                                                        [ class "button is-info is-outlined is-fullwidth"
                                                        , disabled True
                                                        ]
                                                        [ text "No accounts" ]
                                                    ]

                                                Just values ->
                                                    []
                                            )
                                                ++ [ customButton
                                                        [ class "button is-primary is-fullwidth" ]
                                                        [ text "Add new account" ]
                                                   ]
                                        , customButton
                                            [ class "button is-primary is-fullwidth"
                                            , onClick <| ChangeView MainView
                                            ]
                                            [ text "Back" ]
                                        ]
            ]
        ]
    }


mainView model =
    Html.div [] <|
        [ case model.accounts of
            Nothing ->
                customButton
                    [ class "button is-primary is-fullwidth"
                    , disabled True
                    ]
                    [ text "Accounts" ]

            Just accounts ->
                customButton
                    [ class "button is-primary is-fullwidth"
                    , onClick <| ChangeView <| AccountsView accounts
                    ]
                    [ text "Accounts" ]
        , customButton
            [ class "button is-danger is-fullwidth"
            , onClick <| RunCmd Client.signOut
            ]
            [ text "Sign Out" ]
        ]


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
                            FoundDataFile maybeId

                        Err error ->
                            Noop
                )
            , Spreadsheets.response
                (\res ->
                    case res of
                        Ok (Spreadsheets.CreatedSpreadsheet { spreadsheetId }) ->
                            CreatedSpreadsheet { spreadsheetId = spreadsheetId }

                        Ok (Spreadsheets.GotSpreadsheet spreadsheet) ->
                            Noop

                        Ok (Spreadsheets.UpdatedSpreadsheet { spreadsheetId }) ->
                            Noop

                        Err error ->
                            Noop
                )
            , Values.receive
                (\res ->
                    case res of
                        Ok (Values.GotAccounts valueRange) ->
                            UpdatedAccountData { accounts = valueRange }

                        _ ->
                            Noop
                )
            ]

        Just False ->
            []
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


customButton : List (Attribute msg) -> List (Html msg) -> Html msg
customButton attr html =
    Html.button attr html
        |> List.singleton
        |> Html.div
            [ Html.Attributes.class "control"
            , Html.Attributes.style "padding" "3px 0px"
            ]
