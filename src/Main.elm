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
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, height, viewBox, width)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), oneOf, s)


main =
    Browser.application
        { init = init
        , view = viewDocument
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
                        |> Url.toString
                        |> Browser.Navigation.pushUrl mainModel.key
                    )

                Browser.External url ->
                    ( mainModel, Browser.Navigation.load url )

        ChangedUrl url ->
            changeRouteTo (Route.fromUrl mainModel.mountPath url) mainModel



-- View


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = "Random Color"
    , body =
        [ node "base"
            -- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/base
            -- <base> let's use make all relative paths be relative to the mount path.
            [ href
                (if String.endsWith "/" model.mountPath then
                    model.mountPath

                 else
                    model.mountPath ++ "/"
                )
            ]
            []
        , octocat
        , view model
        ]
    }


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
                        , href ("colors/" ++ Color.toHex backgroundColor)
                        , style "display" "block"
                        ]
                        [ text "link to this color" ]
                    , a
                        [ style "color" cssTextColor
                        , href ""
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
                    , href ""
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
                    , href ""
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


octocat : Html msg
octocat =
    a [ href "https://github.com/ravicious/random-color", class "github-corner", attribute "aria-label" "View source on Github" ]
        [ svg [ width "80", height "80", viewBox "0 0 250 250", Svg.Attributes.style "fill:#151513; color:#fff; position: absolute; top: 0; border: 0; right: 0;" ]
            [ path [ d "M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z" ]
                []
            , path [ d "M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2", fill "currentColor", Svg.Attributes.style "transform-origin: 130px 106px;", Svg.Attributes.class "octo-arm" ]
                []
            , path [ d "M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z", fill "currentColor", Svg.Attributes.class "octo-body" ]
                []
            ]
        ]
