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


type Model
    = SpecificColor Color
    | IncorrectColor
    | RandomColor Color


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
        ( RandomColor color, Cmd.none )

    else
        case ColorParser.parse flags.color of
            Ok color ->
                ( SpecificColor color, Cmd.none )

            Err _ ->
                ( IncorrectColor, Cmd.none )


update : msg -> Model -> ( Model, Cmd msg )
update _ model =
    ( model, Cmd.none )



-- View


view : Model -> Html msg
view model =
    case model of
        RandomColor color ->
            let
                backgroundColor =
                    colorToCssRgb color

                textColor =
                    color |> Color.complement |> colorToCssRgb
            in
            layout
                [ style "background-color" backgroundColor
                , style "color" textColor
                ]
                [ text (colorToCssRgb color)
                , br [] []
                , text ("#" ++ Color.toHex color)
                , a
                    [ style "font-size" "calc(.55rem + 1.0vw)"
                    , style "color" textColor
                    , href ("?color=" ++ Color.toHex color)
                    ]
                    [ text "link to this color" ]
                ]

        SpecificColor color ->
            let
                backgroundColor =
                    colorToCssRgb color

                textColor =
                    color |> Color.complement |> colorToCssRgb
            in
            layout
                [ style "background-color" backgroundColor
                , style "color" textColor
                ]
                [ text (colorToCssRgb color)
                , br [] []
                , text ("#" ++ Color.toHex color)
                , a
                    [ style "font-size" "calc(.55rem + 1.0vw)"
                    , style "color" textColor
                    , href "?"
                    ]
                    [ text "generate random color" ]
                ]

        IncorrectColor ->
            layout
                [ style "background-color" "white"
                , style "color" "black"
                ]
                [ text "Incorrect color"
                , a
                    [ style "font-size" "calc(.55rem + 1.0vw)"
                    , href "?"
                    ]
                    [ text "generate random color" ]
                ]


layout : List (Attribute msg) -> List (Html msg) -> Html msg
layout additionalStyles children =
    div
        (List.append
            [ style "height" "100vh"
            , style "width" "100vw"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "font-family" "monospace"
            , style "font-size" "calc(.75rem + 5.5vw)"
            , style "text-align" "center"
            ]
            additionalStyles
        )
        children
