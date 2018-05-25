module Main exposing (main)

import Html exposing (text)


type Msg
    Response (Result Http.Error DriveState)
    | ChangeView View


type View
    = Main
    | Load
    | Save
    | UpdateBalancesView
    | Error


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { state : DriveState
    , view : View
    , updating : Bool
    , error : Maybe String
    , updateBalancesForm : Forms.UpdateBalances.Model
    }


defaultModel : Model
defaultModel =
    { state = { accounts = [] }
    , view = Load
    , updating = True
    , error = Nothing
    , updateBalancesForm = Forms.UpdateBalances.defaultModel
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Http.send Response <| Http.get "/getState" stateDecoder )


view : Model -> Html Msg
view =
    (\model ->
        div [ id "mun33bot-debug-div" ]
            [ div [ id "mun33bot-main-div" ]
                [ h1 [ id "balance-float" ] [ text "$-.--" ]
                , h1 [] [ text "Mun33Bot" ]
                , div [] [ text "loading" ]
                , br [] []
                , span [ id "hourglass" ] [ text <| String.fromChar '⌛' ]
                ]
            , div [ id "mun33bot-state-div" ] [ text (toString model) ]
            ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Response (Ok response) ->
            update (ChangeView Main)
                { model
                    | state = response
                    , updating = False
                    , error = Nothing
                }

        Response (Err error) ->
            update (ChangeView Error) { model | error = Just (toString error) }
