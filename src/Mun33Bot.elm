port module Mun33Bot exposing (main)

import Browser exposing (application)
import Browser.Navigation as Nav
import Char
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
import Parser exposing ((|.), (|=), DeadEnd, Parser)
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

    -- add account
    , addAccountNameState : String
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

      -- add account
      , addAccountNameState = ""
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
      -- add account
    | ChangeAddAccountNameInput String


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

        -- add account
        ChangeAddAccountNameInput newInput ->
            case Parser.run currencyParser newInput of
                Ok _ ->
                    ( { model | addAccountNameState = newInput }, Cmd.none )

                Err deadEnds ->
                    ( model, Cmd.none )


type alias Flags =
    ()


type SpreadsheetId
    = Unknown
    | Missing
    | Id String


type View
    = MainView
    | AccountsView
    | AddAccountView


view : Model -> { title : String, body : List (Node Msg) }
view model =
    { title = "Mun33Bot"
    , body =
        [ section [ id "view", class "section" ]
            [ div [ class "container" ] <|
                case model.signinStatus of
                    Nothing ->
                        [ p [ class "title has-text-centered" ] [ text "Mun33Bot" ]
                        , bulmaText { color = Info, text = "Mun33Bot is loading..." }
                        ]

                    Just False ->
                        [ p [ class "title has-text-centered" ] [ text "Mun33Bot" ]
                        , bulmaText { color = Info, text = "Please authorize this app to use Google..." }
                        , bulmaButton
                            { color = Primary
                            , onClick = Just <| RunCmd Client.signIn
                            , text = "Authorize"
                            }
                        ]

                    Just True ->
                        case model.spreadsheetId of
                            Unknown ->
                                [ p [ class "title has-text-centered" ] [ text "Mun33Bot" ]
                                , bulmaText { color = Info, text = "Loading data..." }
                                , bulmaButton
                                    { color = Danger
                                    , onClick = Just <| RunCmd Client.signOut
                                    , text = "Sign Out"
                                    }
                                ]

                            Missing ->
                                [ p [ class "title has-text-centered" ] [ text "Mun33Bot" ]
                                , bulmaText
                                    { color = Danger
                                    , text = "Failed to load data"
                                    }
                                , bulmaButton
                                    { color = Danger
                                    , onClick = Just <| RunCmd Client.signOut
                                    , text = "Sign Out"
                                    }
                                ]

                            Id spreadsheetId ->
                                [ model
                                    |> (case model.currentView of
                                            MainView ->
                                                mainView

                                            AccountsView ->
                                                accountsView

                                            AddAccountView ->
                                                addAccountView
                                       )
                                ]
            ]
        ]
    }


mainView model =
    Html.div [ id "mainView" ] <|
        [ p [ class "title has-text-centered" ] [ text "Mun33Bot" ]
        , case model.accounts of
            Nothing ->
                bulmaButton
                    { color = Primary
                    , onClick = Nothing
                    , text = "Accounts"
                    }

            Just accounts ->
                bulmaButton
                    { color = Primary
                    , onClick = Just <| ChangeView AccountsView
                    , text = "Accounts"
                    }
        , bulmaButton
            { color = Danger
            , onClick = Just <| RunCmd Client.signOut
            , text = "Sign Out"
            }
        ]


accountsView : Model -> Html Msg
accountsView model =
    Html.div [ id "accountsView" ] <|
        [ p [ class "title has-text-centered" ] [ text "Accounts" ]
        , case model.accounts |> Maybe.andThen .values of
            Nothing ->
                bulmaText { color = Info, text = "No accounts" }

            Just values ->
                div [] []
        , bulmaButton
            { color = Primary
            , onClick = Just <| ChangeView AddAccountView
            , text = "Add new account"
            }
        , bulmaButton
            { color = Primary
            , onClick = Just <| ChangeView MainView
            , text = "Back"
            }
        ]


addAccountView : Model -> Html Msg
addAccountView model =
    Html.div [ id "addAccountView" ] <|
        [ p [ class "title has-text-centered" ] [ text "Add account" ]
        , bulmaInput { label = "Name", placeholder = "Checking account" }
        , bulmaDollarInput
            { label = "Balance"
            , onInput = ChangeAddAccountNameInput
            , value = model.addAccountNameState
            }
        , bulmaButton
            { color = Primary
            , onClick = Just <| ChangeView AddAccountView
            , text = "Back"
            }
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


bulmaButton :
    { onClick : Maybe Msg
    , text : String
    , color : BulmaColor
    }
    -> Html Msg
bulmaButton config =
    div [ class "field" ]
        [ case config.onClick of
            Nothing ->
                div [ class "control" ]
                    [ button
                        [ classList
                            [ ( "button", True )
                            , ( "is-fullwidth", True )
                            , ( toStringBulmaColors config.color, True )
                            ]
                        , disabled True
                        ]
                        [ text config.text ]
                    ]

            Just msg ->
                div [ class "control" ]
                    [ button
                        [ classList
                            [ ( "button", True )
                            , ( "is-fullwidth", True )
                            , ( toStringBulmaColors config.color, True )
                            ]
                        , onClick msg
                        ]
                        [ text config.text ]
                    ]
        ]


bulmaText : { color : BulmaColor, text : String } -> Html msg
bulmaText { color, text } =
    div [ class "field" ]
        [ div [ class "control" ]
            [ input
                [ classList
                    [ ( "input", True )
                    , ( toStringBulmaColors color, True )
                    , ( "has-text-centered", True )
                    ]
                , type_ "text"
                , value text
                , readonly True
                ]
                []
            ]
        ]


bulmaInput : { label : String, placeholder : String } -> Html msg
bulmaInput config =
    div [ class "field" ]
        [ label [ class "label" ] [ text config.label ]
        , div [ class "control" ]
            [ input
                [ class "input"
                , type_ "text"
                , placeholder config.placeholder
                ]
                []
            ]
        ]


bulmaDollarInput :
    { label : String
    , onInput : String -> Msg
    , value : String
    }
    -> Html Msg
bulmaDollarInput config =
    div [ class "field" ]
        [ label [ class "label" ] [ text config.label ]
        , div
            [ classList
                [ ( "control", True )
                , ( "has-icons-left", True )
                ]
            ]
            [ input
                [ class "input"
                , type_ "text"
                , placeholder "100.00"
                , onInput config.onInput
                , value config.value
                ]
                []
            , span
                [ classList
                    [ ( "icon", True )
                    , ( "is-left", True )
                    ]
                ]
                [ i [ class "fas fa-dollar-sign" ] [] ]
            ]
        ]


type BulmaColor
    = Primary
    | Link
    | Info
    | Success
    | Warning
    | Danger


toStringBulmaColors : BulmaColor -> String
toStringBulmaColors bc =
    case bc of
        Primary ->
            "is-primary"

        Link ->
            "is-link"

        Info ->
            "is-info"

        Success ->
            "is-success"

        Warning ->
            "is-warning"

        Danger ->
            "is-danger"


currencyParser : Parser Float
currencyParser =
    Parser.oneOf
        [ Parser.succeed 0.0
            |. Parser.end
        , Parser.succeed identity
            |. Parser.symbol "."
            |= Parser.oneOf
                [ Parser.succeed 0.0
                    |. Parser.end
                , Parser.succeed (\cents -> cents / 100)
                    |= centsParser
                    |. Parser.end
                ]
        , Parser.succeed (\dollars cents -> toFloat dollars + cents / 100)
            |= dollarParser
            |= Parser.oneOf
                [ Parser.succeed 0.0
                    |. Parser.end
                , Parser.succeed identity
                    |. Parser.symbol "."
                    |= Parser.oneOf
                        [ Parser.succeed 0.0
                            |. Parser.end
                        , Parser.succeed (\cents -> cents / 100.0)
                            |= centsParser
                            |. Parser.end
                        ]
                ]
        ]


dollarParser : Parser Int
dollarParser =
    (Parser.getChompedString <| Parser.chompWhile Char.isDigit)
        |> Parser.andThen
            (\intStr ->
                case String.toInt intStr of
                    Just int ->
                        Parser.succeed int

                    Nothing ->
                        Parser.problem "could not convert dollars to integer"
            )


centsParser : Parser Float
centsParser =
    (Parser.getChompedString <|
        (Parser.chompIf Char.isDigit
            |> Parser.andThen
                (always
                    (Parser.oneOf
                        [ Parser.end
                        , Parser.chompIf Char.isDigit
                        ]
                    )
                )
        )
    )
        |> Parser.andThen
            (\intStr ->
                case String.toInt intStr of
                    Just int ->
                        Parser.succeed <| toFloat int / 100.0

                    Nothing ->
                        Parser.problem "could not convert cents to integer"
            )
