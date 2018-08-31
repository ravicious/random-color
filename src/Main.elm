module Main exposing (..)

import Random
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Color exposing (Color)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


colorToCssRgb : Color -> String
colorToCssRgb color =
    let
        { red, green, blue } =
            Color.toRgb color
    in
        ("rgb("
            ++ (String.fromInt red)
            ++ ", "
            ++ (String.fromInt green)
            ++ ", "
            ++ (String.fromInt blue)
            ++ ")"
        )


type alias Model =
    Maybe Color


byteGenerator : Random.Generator Int
byteGenerator =
    Random.int 0 255


colorGenerator : Random.Generator Color
colorGenerator =
    Random.map3 Color.rgb byteGenerator byteGenerator byteGenerator


init : () -> ( Model, Cmd Msg )
init flags =
    ( Nothing, Random.generate RandomColor colorGenerator )


type Msg
    = RandomColor Color


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomColor color ->
            ( Just color, Cmd.none )


view model =
    case model of
        Nothing ->
            text ""

        Just color ->
            div
                [ style "height" "100vh"
                , style "width" "100vw"
                , style "background-color" (colorToCssRgb color)
                , style "color" (color |> Color.complement |> colorToCssRgb)
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "font-family" "monospace"
                , style "font-size" "calc(.75rem + 5.5vw)"
                , style "text-align" "center"
                ]
                [ text (colorToCssRgb color)
                , br [] []
                , text ("#" ++ (Color.toHex color))
                ]
