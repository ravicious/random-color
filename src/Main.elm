module Main exposing (main)

import Browser
import Browser.Navigation
import Color exposing (Color)
import ColorParser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Route
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), oneOf, s)


main =
    Browser.application
        { init = init
        , view = Browser.Document "Random Color" << List.singleton << view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }


type alias Model =
    { key : Browser.Navigation.Key
    , page : Page
    , seed : Random.Seed
    , mountPath : String
    }


type Page
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

    -- A string describing the path under which the root of the app is mounted, like "/foo" or
    -- "/foo/bar". If the app is mounted under the root of the domain, `mountPath` can be set to "".
    , mountPath : String
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


generateRandomPageColors : Random.Seed -> ( PageColors, Random.Seed )
generateRandomPageColors seed =
    let
        ( backgroundColor, nextSeed ) =
            Random.step colorGenerator seed

        textColor =
            findContrastingOrComplementaryColor backgroundColor
    in
    ( { backgroundColor = backgroundColor
      , textColor = textColor
      }
    , nextSeed
    )


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        seed =
            Random.initialSeed flags.randomNumber
    in
    changeRouteTo (Route.fromUrl flags.mountPath url)
        { page = IncorrectColor
        , key = key
        , seed = seed
        , mountPath = flags.mountPath
        }


changeRouteTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute mainModel =
    case maybeRoute of
        Nothing ->
            ( { mainModel | page = IncorrectColor }, Cmd.none )

        Just (Route.IncorrectColor _) ->
            ( { mainModel | page = IncorrectColor }, Cmd.none )

        Just (Route.SpecificColor color) ->
            ( { mainModel
                | page =
                    SpecificColor
                        { backgroundColor = color
                        , textColor = findContrastingOrComplementaryColor color
                        }
              }
            , Cmd.none
            )

        Just Route.RandomColor ->
            let
                ( pageColors, nextSeed ) =
                    generateRandomPageColors mainModel.seed
            in
            ( { mainModel
                | page =
                    RandomColor pageColors
                , seed = nextSeed
              }
            , Cmd.none
            )


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mainModel =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( mainModel
                    , url
                        |> Route.addMountPathToUrl mainModel.mountPath
                        |> Url.toString
                        |> Browser.Navigation.pushUrl mainModel.key
                    )

                Browser.External url ->
                    ( mainModel, Browser.Navigation.load url )

        ChangedUrl url ->
            changeRouteTo (Route.fromUrl mainModel.mountPath url) mainModel



-- View


view : Model -> Html Msg
view mainModel =
    case mainModel.page of
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
                , div
                    [ style "font-size" "calc(.55rem + 1.0vw)"
                    , style "line-height" "1.5em"
                    ]
                    [ a
                        [ style "color" cssTextColor
                        , href ("/colors/" ++ Color.toHex backgroundColor)
                        , style "display" "block"
                        ]
                        [ text "link to this color" ]
                    , a
                        [ style "color" cssTextColor
                        , href "/"
                        ]
                        [ text "generate random color" ]
                    ]
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
                    , href "/"
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
                    , href "/"
                    ]
                    [ text "generate random color" ]
                ]


layout : List (Attribute Msg) -> List (Html Msg) -> Html Msg
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
