module Route exposing (Route(..), fromUrl)

import Color exposing (Color)
import ColorParser
import Regex
import Url exposing (Url)
import Url.Parser exposing ((</>), oneOf, s)


type Route
    = SpecificColor Color
    | RandomColor
    | IncorrectColor String


fromUrl : String -> Url -> Maybe Route
fromUrl mountPath =
    removeMountPathFromUrl mountPath >> parseUrl


parseUrl : Url -> Maybe Route
parseUrl =
    Url.Parser.parse <|
        oneOf
            [ Url.Parser.map RandomColor Url.Parser.top
            , Url.Parser.map SpecificColor (s "colors" </> ColorParser.urlParser)
            , Url.Parser.map IncorrectColor (s "colors" </> Url.Parser.string)
            ]


removeMountPathFromUrl : String -> Url -> Url
removeMountPathFromUrl mountPath url =
    let
        mountPathRegex =
            Maybe.withDefault Regex.never <| Regex.fromString <| "^" ++ mountPath

        newPath =
            Regex.replace mountPathRegex (\_ -> "") url.path
    in
    { url | path = newPath }
