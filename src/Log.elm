port module Log exposing (LogLevel(..), Model, Msg(..), default, subscriptions, update, view)

import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Process exposing (..)
import Task exposing (perform)
import Time exposing (Posix, every, now, posixToMillis)


type alias Flags =
    { displayTime : Int
    , tickTime : Int
    , purgeTime : Int
    , logSize : Int
    }


type alias Model =
    { flags : Flags
    , filter : String
    , log : Dict Int ( LogLevel, String )
    , time : Int
    , subCount : Int
    }


type LogLevel
    = Info
    | Warn
    | Error


default : Model
default =
    { flags =
        { displayTime = 15000
        , tickTime = 500
        , purgeTime = 180000
        , logSize = 500
        }
    , filter = ""
    , log = Dict.empty
    , time = 0
    , subCount = 0
    }


type Msg
    = AddToLog LogLevel String Posix
    | ChangeFilter String
    | Purge Posix
    | ScheduleAddToLog LogLevel String
    | Tick Posix
    | AddToSubCount
    | RemoveFromSubCount


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddToLog logLevel message time ->
            update AddToSubCount
                { model
                    | log =
                        Dict.insert (posixToMillis time)
                            ( logLevel, message )
                            model.log
                    , time = posixToMillis time
                }

        ChangeFilter filter ->
            ( { model | filter = filter }, Cmd.none )

        Purge _ ->
            if Dict.size model.log > model.flags.logSize then
                ( { model
                    | log =
                        Dict.keys model.log
                            |> List.reverse
                            |> List.take model.flags.logSize
                            |> List.map (\k -> Tuple.pair k ( Info, "" ))
                            |> Dict.fromList
                            |> Dict.intersect model.log
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ScheduleAddToLog message logLevel ->
            ( model, perform (AddToLog message logLevel) now )

        Tick posixTime ->
            ( { model | time = posixToMillis posixTime }, Cmd.none )

        AddToSubCount ->
            ( { model | subCount = model.subCount + 1 }
            , perform (\_ -> RemoveFromSubCount) <| sleep <| toFloat <| model.flags.displayTime + 2 * model.flags.tickTime
            )

        RemoveFromSubCount ->
            ( { model | subCount = model.subCount - 1 }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "search", value model.filter, onInput ChangeFilter ] []
        , logDiv model
        ]


logDiv : Model -> Html Msg
logDiv model =
    logDivDict model <|
        case model.filter of
            "" ->
                Dict.filter (\k _ -> k > model.time - model.flags.displayTime) model.log

            otherwise ->
                Dict.filter (\_ ( _, v ) -> String.contains model.filter v) model.log


logDivDict : Model -> Dict Int ( LogLevel, String ) -> Html Msg
logDivDict model dict =
    let
        millisecsPerSecond =
            1000

        millisecsPerMinute =
            60000
    in
    div [] <|
        if model.filter /= "" && Dict.isEmpty dict then
            [ p [] [ Html.Styled.em [] [ text "no results" ] ] ]

        else
            List.reverse <|
                Dict.values <|
                    Dict.map
                        (\time ( logLevel, message ) ->
                            String.split model.filter message
                                |> List.map text
                                |> List.intersperse (b [] [ text model.filter ])
                                |> (++)
                                    [ Html.Styled.small [] <|
                                        let
                                            longAgo =
                                                model.time - time
                                        in
                                        if longAgo < millisecsPerMinute then
                                            [ longAgo
                                                // millisecsPerSecond
                                                |> String.fromInt
                                                |> text
                                            , text " sec ago: "
                                            ]

                                        else
                                            [ longAgo
                                                // millisecsPerSecond
                                                |> String.fromInt
                                                |> text
                                            , text " min ago: "
                                            ]
                                    ]
                                |> p
                                    [ css
                                        [ color <|
                                            case logLevel of
                                                Info ->
                                                    hex "#000000"

                                                Warn ->
                                                    hex "#ffcc00"

                                                Error ->
                                                    hex "#ff0000"
                                        ]
                                    ]
                        )
                        dict


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.batch
            [ logInfo <| ScheduleAddToLog Info
            , logWarning <| ScheduleAddToLog Warn
            , logError <| ScheduleAddToLog Error
            ]
        , Sub.batch <|
            if model.subCount > 0 || String.length model.filter > 0 then
                [ every (toFloat model.flags.tickTime) Tick
                , every (toFloat model.flags.purgeTime) Purge
                ]

            else
                []
        ]


port logInfo : (String -> msg) -> Sub msg


port logWarning : (String -> msg) -> Sub msg


port logError : (String -> msg) -> Sub msg
