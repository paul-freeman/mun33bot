module Mun33Bot exposing (..)

{-| The main Mun33Bot code.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Task
import Process
import Json.Decode
import Json.Encode
import DriveState exposing (DriveState)
import Helpers
import Forms.UpdateBalances exposing (defaultModel)


-- MUN33BOT MODEL
-- The Mun33Bot model contains the permanent state, which is saved on Google
-- Drive, as well as all the other variables to handle each user session.


balance : Model -> List (Html Msg)
balance model =
    [ h1 [ id "balance-float" ] <|
        if model.updating then
            [ text "$-.--" ]
        else
            [ List.map Tuple.second model.state.accounts
                |> List.sum
                |> Helpers.toCurrencyString
                |> Html.text
            ]
    ]


mainView : Model -> List (Html Msg)
mainView model =
    [ Html.button
        [ onClick (ChangeView UpdateBalancesView) ]
        [ text "accounts" ]
    ]


view : Model -> List (Html Msg)
view model =
    case model.view of
        Main ->
            mainView model

        Load ->
            [ div []
                [ text "loading"
                , br [] []
                , span [ id "hourglass" ] [ text <| String.fromChar '⌛' ]
                ]
            ]

        Save ->
            [ div []
                [ text "saving"
                , br [] []
                , span [ id "hourglass" ] [ text <| String.fromChar '⌛' ]
                ]
            ]

        UpdateBalancesView ->
            Forms.UpdateBalances.view UpdateBalancesCallback model.updateBalancesForm

        Error ->
            [ div []
                [ text
                    (case model.error of
                        Nothing ->
                            "Error: no error"

                        Just err ->
                            err
                    )
                ]
            ]


noOp : Model -> ( Model, Cmd Msg )
noOp =
    update NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetState ->
            ( { model | updating = True }, Http.send Response getRequest )

        ChangeView view ->
            case view of
                Main ->
                    noOp { model | view = view }

                Load ->
                    update GetState { model | view = view }

                UpdateBalancesView ->
                    noOp
                        { model
                            | view = view
                            , updateBalancesForm =
                                { defaultModel
                                    | accounts =
                                        List.map
                                            (\( k, v ) -> ( k, toString v ))
                                            model.state.accounts
                                }
                        }

                Error ->
                    noOp { model | view = view }

        UpdateBalancesCallback subMsg ->
            case subMsg of
                Forms.UpdateBalances.Submit form ->
                    update PostStateNoWait { model | state = DriveState form }

                Forms.UpdateBalances.Cancel ->
                    update (ChangeView Main) model

                otherwise ->
                    noOp
                        { model
                            | updateBalancesForm =
                                Forms.UpdateBalances.update subMsg model.updateBalancesForm
                        }


postRequest : DriveState -> Http.Request DriveState
postRequest state =
    let
        body =
            stateEncoder state |> Http.jsonBody
    in
        Http.post saveStateAddress body stateDecoder


postRequestNoWait : DriveState -> Cmd Msg
postRequestNoWait state =
    postRequest state
        |> Http.toTask
        |> Process.spawn
        |> Task.perform (\_ -> ChangeView Main)
