module Mun33BotTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import Helpers


suite : Test
suite =
    describe "Mun33Bot tests"
        [ describe "Mun33Bot.matchFirst"
            [ fuzz2 Fuzz.string Fuzz.float "does it match" <|
                \a b ->
                    Expect.true "Expect match" <|
                        Helpers.matchFirst a ( a, b )
            , fuzz3 Fuzz.string Fuzz.string Fuzz.float "does it not match" <|
                \a1 a2 b ->
                    Expect.false "Expect no match" <|
                        if a1 == a2 then
                            False
                        else
                            Helpers.matchFirst a1 ( a2, b )
            ]
        ]
