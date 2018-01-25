module Helpers exposing (..)


toCurrency : String -> String
toCurrency s =
    case String.uncons s of
        Nothing ->
            s

        Just ( c, rest ) ->
            let
                s_ =
                    if c == '$' then
                        rest
                    else
                        String.cons c rest
            in
                case String.toFloat s_ of
                    Err _ ->
                        s_

                    Ok f ->
                        toCurrencyString f


toCurrencyString : Float -> String
toCurrencyString x =
    let
        x100 =
            round (100 * x)

        dollars =
            toString <| x100 // 100

        cents =
            String.padLeft 2 '0' <| toString <| rem x100 100
    in
        "$" ++ dollars ++ "." ++ cents
