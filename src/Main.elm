module Main exposing (main)

import Browser
import Color exposing (Color)
import ColorParser
import Html exposing (..)
import Html.Attributes exposing (..)
import Random


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


colorToCssRgb : Color -> String
colorToCssRgb color =
    let
        { red, green, blue } =
            Color.toRgb color
    in
    "rgb("
        ++ String.fromInt red
        ++ ", "
        ++ String.fromInt green
        ++ ", "
        ++ String.fromInt blue
        ++ ")"


type alias Model =
    Maybe Color


byteGenerator : Random.Generator Int
byteGenerator =
    Random.int 0 255


colorGenerator : Random.Generator Color
colorGenerator =
    Random.map3 Color.rgb byteGenerator byteGenerator byteGenerator


type alias Flags =
    { color : String
    , randomNumber : Int
    }


init : Flags -> ( Model, Cmd msg )
init flags =
    if String.isEmpty flags.color then
        let
            seed =
                Random.initialSeed flags.randomNumber

            ( color, _ ) =
                Random.step colorGenerator seed
        in
        ( Just color, Cmd.none )

    else
        case ColorParser.parse flags.color of
            Ok color ->
                ( Just color, Cmd.none )

            Err _ ->
                ( Nothing, Cmd.none )


update : msg -> Model -> ( Model, Cmd msg )
update _ model =
    ( model, Cmd.none )



-- View


view : Model -> Html msg
view model =
    case model of
        Nothing ->
            layout
                [ style "background-color" "white"
                , style "color" "black"
                ]
                [ text "Incorrect color"
                ]

        Just color ->
            layout
                [ style "background-color" (colorToCssRgb color)
                , style "color" (color |> Color.complement |> colorToCssRgb)
                ]
                [ text (colorToCssRgb color)
                , br [] []
                , text ("#" ++ Color.toHex color)
                ]


layout : List (Attribute msg) -> List (Html msg) -> Html msg
layout additionalStyles children =
    div
        (List.append
            [ style "height" "100vh"
            , style "width" "100vw"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "font-family" "monospace"
            , style "font-size" "calc(.75rem + 5.5vw)"
            , style "text-align" "center"
            ]
            additionalStyles
        )
        children
