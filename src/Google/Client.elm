port module Google.Client exposing (Request, Response, signIn, signOut)

import Json.Encode as E exposing (Value)


type alias Request =
    { method : String
    , args : List Value
    }


type alias Response =
    { method : String
    , args : List Value
    , response : Value
    }


signIn =
    gapiAuth2GetAuthInstance
        { method = "signIn"
        , args = []
        }


signOut =
    gapiAuth2GetAuthInstance
        { method = "signOut"
        , args = []
        }


port gapiAuth2GetAuthInstance : Request -> Cmd msg
