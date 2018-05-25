module DriveState exposing (DriveState, stateEncoder, stateDecoder)

import Json.Encode
import Json.Decode
import List exposing (sort)
import Tuple exposing (second)


type alias DriveState =
    { accounts : List ( String, Float ) }


stateEncoder : DriveState -> Json.Encode.Value
stateEncoder state =
    Json.Encode.object [ ( "accounts", accountsEncoder state.accounts ) ]


stateDecoder : Json.Decode.Decoder DriveState
stateDecoder =
    Json.Decode.map
        (\h -> { accounts = List.map second (sort h) })
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
