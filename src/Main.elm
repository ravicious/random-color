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
        , subscriptions = always Sub.none
        }


type Model
    = IncorrectColor
    | SpecificColor PageColors
    | RandomColor PageColors


type alias PageColors =
    { backgroundColor : Color
    , textColor : Color
    }


type alias Flags =
    { color : String
    , randomNumber : Int
    }


byteGenerator : Random.Generator Int
byteGenerator =
    Random.int 0 255


colorGenerator : Random.Generator Color
colorGenerator =
    Random.map3 Color.rgb byteGenerator byteGenerator byteGenerator


generateContrastingColor : Float -> Int -> Random.Seed -> Color -> ( Maybe Color, Random.Seed )
generateContrastingColor minimumContrast maxTries seed color =
    if maxTries == 0 then
        ( Nothing, seed )

    else
        let
            ( nextColor, nextSeed ) =
                Random.step colorGenerator seed
        in
        if Color.contrast color nextColor >= minimumContrast then
            ( Just nextColor, nextSeed )

        else
            generateContrastingColor minimumContrast (maxTries - 1) nextSeed color


findContrastingOrComplementaryColor : Color -> Color
findContrastingOrComplementaryColor color =
    let
        complementaryColor =
            Color.complement color

        -- Create the seed based on the given color, so that for any given color we're always
        -- going to find the same contrasting color.
        seed =
            Random.initialSeed (Color.toRgbInt color)
    in
    generateContrastingColor 3.0 50 seed color
        |> Tuple.first
        |> Maybe.withDefault complementaryColor


init : Flags -> ( Model, Cmd msg )
init flags =
    if String.isEmpty flags.color then
        let
            ( backgroundColor, _ ) =
                Random.step colorGenerator (Random.initialSeed flags.randomNumber)

            textColor =
                findContrastingOrComplementaryColor backgroundColor
        in
        ( RandomColor
            { backgroundColor = backgroundColor
            , textColor = textColor
            }
        , Cmd.none
        )

    else
        case ColorParser.parse flags.color of
            Ok color ->
                ( SpecificColor
                    { backgroundColor = color
                    , textColor = findContrastingOrComplementaryColor color
                    }
                , Cmd.none
                )

            Err _ ->
                ( IncorrectColor, Cmd.none )


update : msg -> Model -> ( Model, Cmd msg )
update _ model =
    ( model, Cmd.none )



-- View


view : Model -> Html msg
view model =
    case model of
        RandomColor { backgroundColor, textColor } ->
            let
                cssBackgroundColor =
                    colorToCssRgb backgroundColor

                cssTextColor =
                    colorToCssRgb textColor
            in
            layout
                [ style "background-color" cssBackgroundColor
                , style "color" cssTextColor
                ]
                [ text (colorToCssRgb backgroundColor)
                , br [] []
                , text ("#" ++ Color.toHex backgroundColor)
                , a
                    [ style "font-size" "calc(.55rem + 1.0vw)"
                    , style "color" cssTextColor
                    , href ("?color=" ++ Color.toHex backgroundColor)
                    ]
                    [ text "link to this color" ]
                ]

        SpecificColor { backgroundColor, textColor } ->
            let
                cssBackgroundColor =
                    colorToCssRgb backgroundColor

                cssTextColor =
                    colorToCssRgb textColor
            in
            layout
                [ style "background-color" cssBackgroundColor
                , style "color" cssTextColor
                ]
                [ text (colorToCssRgb backgroundColor)
                , br [] []
                , text ("#" ++ Color.toHex backgroundColor)
                , a
                    [ style "font-size" "calc(.55rem + 1.0vw)"
                    , style "color" cssTextColor
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
