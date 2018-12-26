module Route exposing (Route(..), fromUrl)

import Color exposing (Color)
import ColorParser
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), oneOf, s)


type Route
    = SpecificColor Color
    | RandomColor
    | IncorrectColor String


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse <|
        oneOf
            [ Parser.map RandomColor Parser.top
            , Parser.map SpecificColor (s "colors" </> ColorParser.urlParser)
            , Parser.map IncorrectColor (s "colors" </> Parser.string)
            ]
