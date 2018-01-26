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


type alias Model =
    { state : DriveState
    , view : View
    , updating : Bool
    , error : Maybe String
    , updateBalancesForm : Forms.UpdateBalances.Model
    }


type Msg
    = NoOp
    | GetState
    | PostState
    | PostStateNoWait
    | Response (Result Http.Error DriveState)
    | ChangeView View
    | UpdateBalancesCallback Forms.UpdateBalances.Msg


type View
    = Main
    | Load
    | Save
    | UpdateBalancesView


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

        PostState ->
            ( { model | updating = True }, Http.send Response (postRequest model.state) )

        PostStateNoWait ->
            ( model, postRequestNoWait model.state )

        ChangeView view ->
            case view of
                Main ->
                    noOp { model | view = view }

                Load ->
                    update GetState { model | view = view }

                Save ->
                    update PostState { model | view = view }

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

        Response (Ok response) ->
            update (ChangeView Main)
                { model
                    | state = response
                    , updating = False
                    , error = Nothing
                }

        Response (Err error) ->
            update (ChangeView Save) { model | error = Just (toString error) }

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


main : Program Never Model Msg
main =
    Html.program
        { init =
            update (ChangeView Load)
                { state = { accounts = [] }
                , view = Load
                , updating = True
                , error = Nothing
                , updateBalancesForm = Forms.UpdateBalances.defaultModel
                }
        , view =
            (\model ->
                div [ id "mun33bot-debug-div" ]
                    [ div [ id "mun33bot-main-div" ]
                        (balance model ++ [ h1 [] [ text "Mun33Bot" ] ] ++ view model)
                    , div [ id "mun33bot-state-div" ] [ text (toString model) ]
                    ]
            )
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


getRequest : Http.Request DriveState
getRequest =
    Http.get stateAddress stateDecoder


postRequest : DriveState -> Http.Request DriveState
postRequest state =
    let
        body =
            stateEncoder state |> Http.jsonBody
    in
        Http.post stateAddress body stateDecoder


postRequestNoWait : DriveState -> Cmd Msg
postRequestNoWait state =
    postRequest state
        |> Http.toTask
        |> Process.spawn
        |> Task.perform (\_ -> ChangeView Main)


stateEncoder : DriveState -> Json.Encode.Value
stateEncoder state =
    Json.Encode.object
        [ ( "accounts", accountsEncoder state.accounts ) ]


accountsEncoder : List ( String, Float ) -> Json.Encode.Value
accountsEncoder accounts =
    Json.Encode.list <|
        List.indexedMap (\i ( k, v ) -> accountEncoder i k v) accounts


accountEncoder : Int -> String -> Float -> Json.Encode.Value
accountEncoder index description balance =
    Json.Encode.object
        [ ( "index", Json.Encode.int index )
        , ( "description", Json.Encode.string description )
        , ( "balance", Json.Encode.float balance )
        ]


stateDecoder : Json.Decode.Decoder DriveState
stateDecoder =
    Json.Decode.map
        (\h -> { accounts = List.map Tuple.second (List.sort h) })
        (Json.Decode.field
            "accounts"
            (Json.Decode.list
                (Json.Decode.map3
                    (\i d b -> ( i, ( d, b ) ))
                    (Json.Decode.field "index" Json.Decode.int)
                    (Json.Decode.field "description" Json.Decode.string)
                    (Json.Decode.field "balance" Json.Decode.float)
                )
            )
        )


stateAddress : String
stateAddress =
    "/getState"
