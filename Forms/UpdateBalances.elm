module Forms.UpdateBalances exposing (..)

import Result exposing (toMaybe)
import Maybe exposing (andThen)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import DriveState


type alias Model =
    { accounts : List ( String, String )
    , delete : Bool
    }


type alias Validated =
    List ( String, Float )


type Msg
    = UpdateBalance String String
    | UpdateDescription String String
    | AddAccount String
    | DeleteAccount String
    | ChangeView
    | Submit Validated
    | Cancel


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddAccount new ->
            if List.filter (\( k, v ) -> k == new) model.accounts /= [] then
                update (AddAccount <| String.cons '*' new) model
            else
                { model | accounts = List.append model.accounts [ ( new, "0.00" ) ] }

        DeleteAccount name ->
            { model | accounts = List.filter (\( k, v ) -> k /= name) model.accounts }
                |> update ChangeView

        ChangeView ->
            { model | delete = not model.delete }

        UpdateDescription old new_ ->
            let
                new =
                    if String.length new_ > 18 then
                        String.left 18 new_
                    else
                        new_
            in
                if List.filter (\( k, v ) -> k == new) model.accounts /= [] then
                    update (UpdateDescription old <| String.cons '*' new) model
                else
                    { model
                        | accounts =
                            List.map
                                (\( k, v ) ->
                                    if k == old then
                                        ( new, v )
                                    else
                                        ( k, v )
                                )
                                model.accounts
                    }

        UpdateBalance desc bal_str ->
            let
                bal =
                    case String.uncons bal_str of
                        Nothing ->
                            -- should never happen
                            ""

                        Just ( c, rest ) ->
                            let
                                str =
                                    if c == '$' then
                                        rest
                                    else
                                        String.cons c rest
                            in
                                case String.toFloat str of
                                    Ok float ->
                                        let
                                            parts =
                                                List.take 2 <| String.split "." str

                                            dollars =
                                                List.head parts

                                            dol_str =
                                                case dollars of
                                                    Nothing ->
                                                        -- should never happen
                                                        "0"

                                                    Just "" ->
                                                        "0"

                                                    Just string ->
                                                        case String.toInt string of
                                                            Ok int ->
                                                                toString int

                                                            Err err ->
                                                                string

                                            decimal =
                                                List.tail parts
                                                    |> Maybe.andThen List.head
                                        in
                                            case decimal of
                                                Nothing ->
                                                    dol_str

                                                Just "" ->
                                                    dol_str ++ "."

                                                Just dec_str ->
                                                    dol_str ++ "." ++ (String.left 2 dec_str)

                                    Err _ ->
                                        str
            in
                { model
                    | accounts =
                        List.map
                            (\( k, v ) ->
                                if k == desc then
                                    ( k, bal )
                                else
                                    ( k, v )
                            )
                            model.accounts
                }

        otherwise ->
            model


view : (Msg -> msg) -> Model -> List (Html msg)
view callback model =
    List.concat (List.indexedMap (subView model.delete callback) model.accounts)
        ++ case validate model of
            Ok val ->
                if model.delete then
                    [ Html.button [ onClick (callback ChangeView) ] [ text "cancel" ] ]
                else
                    [ Html.button [ onClick (callback (AddAccount "new")) ] [ text "add" ]
                    , Html.button [ onClick (callback ChangeView) ] [ text "delete" ]
                    , Html.button [ onClick (callback (Submit val)) ] [ text "save" ]
                    , Html.button [ onClick (callback Cancel) ] [ text "cancel" ]
                    ]

            Err err ->
                [ text err ]


validate : Model -> Result String Validated
validate model =
    let
        f val =
            Result.andThen (\_ -> String.toFloat val)
    in
        case List.foldr f (Ok 0.0) (List.map Tuple.second model.accounts) of
            Err err ->
                Err err

            Ok _ ->
                List.map (\( k, v ) -> ( k, Result.withDefault 0.0 <| String.toFloat v ))
                    model.accounts
                    |> Ok


subView : Bool -> (Msg -> msg) -> Int -> ( String, String ) -> List (Html msg)
subView deleteView callback num account =
    let
        description =
            Tuple.first account

        balance =
            Tuple.second account
    in
        [ div [ class "account" ]
            (if deleteView then
                [ button
                    [ class "account-delete-button"
                    , onClick (callback <| DeleteAccount description)
                    ]
                    [ text (description ++ " " ++ balance) ]
                ]
             else
                [ input
                    [ class "account-description"
                    , type_ "text"
                    , value description
                    , onInput (callback << UpdateDescription description)
                    ]
                    []
                , input
                    [ class "account-balance"
                    , type_ "text"
                    , value <| String.cons '$' balance
                    , onInput (callback << UpdateBalance description)
                    ]
                    []
                ]
            )
        ]
