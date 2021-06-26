module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Att
import Html.Events exposing (onInput, onSubmit)
import Http
import ISO8601
import Json.Decode exposing (Decoder, field, int, list, map2, string)
import Url
import Url.Builder
import Url.Parser exposing ((</>))


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = Navigate
        }


type Route
    = ZipCode String


routeParser : Url.Parser.Parser (Route -> a) a
routeParser =
    Url.Parser.map ZipCode (Url.Parser.s "zipcode" </> Url.Parser.string)


type Forcast
    = Idle
    | Failure
    | Loading
    | Success (List HourForcast)


type alias Model =
    { zipCode : String
    , zipCodeInput : String
    , forcast : Forcast
    , navKey : Nav.Key
    }


type alias HourForcast =
    { hour : Hour
    , uv : Int
    }


type alias Hour =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    case Url.Parser.parse routeParser url of
        Just (ZipCode zip) ->
            ( { zipCode = zip
              , zipCodeInput = zip
              , forcast = Loading
              , navKey = key
              }
            , getForcast zip
            )

        Nothing ->
            ( { zipCode = ""
              , zipCodeInput = ""
              , forcast = Idle
              , navKey = key
              }
            , Cmd.none
            )


type Msg
    = GotForcast (Result Http.Error ForcastData)
    | ZipCodeEntry String
    | ZipCodeSubmit
    | UrlChanged Url.Url
    | Navigate Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotForcast result ->
            case result of
                Ok forcastData ->
                    ( { model | forcast = Success (List.map parseHourData forcastData) }, Cmd.none )

                Err _ ->
                    ( { model | forcast = Failure }, Cmd.none )

        ZipCodeEntry input ->
            ( { model | zipCodeInput = input }, Cmd.none )

        ZipCodeSubmit ->
            ( { model | forcast = Loading }
            , switchZipCode model.navKey model.zipCodeInput
            )

        UrlChanged url ->
            case Url.Parser.parse routeParser url of
                Just (ZipCode zip) ->
                    ( { model | zipCode = zip }, getForcast zip )

                Nothing ->
                    ( { model | zipCode = "" }, Cmd.none )

        Navigate urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )


subscriptions : Model -> Sub Msg
subscriptions mode =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "UV Index"
    , body =
        [ div [ Att.class "container" ]
            [ Html.form [ onSubmit ZipCodeSubmit, Att.class "location-form" ]
                [ label [ Att.for "zip-code-input" ] [ text "Zip Code:" ]
                , input
                    [ Att.id "zip-code-input"
                    , Att.type_ "text"
                    , Att.value model.zipCodeInput
                    , onInput ZipCodeEntry
                    ]
                    []
                , button [ Att.type_ "submit" ] [ text "Submit" ]
                ]
            , forcastDisplay model.forcast
            ]
        ]
    }


forcastDisplay : Forcast -> Html Msg
forcastDisplay forcastState =
    case forcastState of
        Idle ->
            text ""

        Failure ->
            text "Error"

        Loading ->
            text "Loading..."

        Success forcast ->
            uvGrid forcast


uvGrid forcast =
    table [ Att.class "uvTable" ]
        [ thead []
            [ tr []
                [ th [ Att.class "uvHour" ] [ text "Time" ]
                , th [ Att.class "uvHour" ] [ text "UV" ]
                ]
            ]
        , tbody []
            (List.map uvRow forcast)
        ]


uvRow : HourForcast -> Html Msg
uvRow data =
    tr []
        [ td [ Att.class "uvHour uvCell" ] [ text (formatHour data.hour) ]
        , td [ Att.class "uvHourValue uvCell" ] [ text (String.fromInt data.uv) ]
        ]


formatHour : Hour -> String
formatHour hour =
    let
        monthStr =
            String.fromInt hour.month

        dayStr =
            String.fromInt hour.day

        hourStr =
            if hour.hour <= 12 then
                String.fromInt hour.hour

            else
                String.fromInt (remainderBy 12 hour.hour)

        ampm =
            if hour.hour < 12 then
                "am"

            else
                "pm"
    in
    monthStr ++ "/" ++ dayStr ++ " " ++ hourStr ++ ":00 " ++ ampm



-- HTTP


parseHourData : HourData -> HourForcast
parseHourData data =
    let
        parsedTime =
            ISO8601.fromString data.datetime
    in
    case parsedTime of
        Ok time ->
            { hour =
                { year = time.year
                , month = time.month
                , day = time.day
                , hour = time.hour
                }
            , uv = data.uv
            }

        Err _ ->
            { hour =
                { year = 0
                , month = 1
                , day = 1
                , hour = 0
                }
            , uv = data.uv
            }


type alias HourData =
    { datetime : String
    , uv : Int
    }


type alias ForcastData =
    List HourData


getForcast : String -> Cmd Msg
getForcast zipCode =
    Http.get
        { url = "https://uv-api.fly.dev/zipcode/" ++ zipCode
        , expect = Http.expectJson GotForcast forcastDecoder
        }


switchZipCode : Nav.Key -> String -> Cmd Msg
switchZipCode key zip =
    Nav.pushUrl key ("/zipcode/" ++ zip)


forcastDecoder : Decoder ForcastData
forcastDecoder =
    field "hourly" (list hourDecoder)


hourDecoder : Decoder HourData
hourDecoder =
    map2 HourData
        (field
            "datetime"
            string
        )
        (field "uv" int)
