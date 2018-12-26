module Route exposing (Route(..), addMountPathToUrl, fromUrl)

import Color exposing (Color)
import ColorParser
import Parser exposing ((|.))
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
        parser =
            Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.token mountPath
                    |. Parser.chompUntilEndOr "\n"

        newPath =
            Parser.run parser url.path
                |> Result.withDefault url.path
    in
    { url | path = newPath }


addMountPathToUrl : String -> Url -> Url
addMountPathToUrl mountPath url =
    { url | path = mountPath ++ url.path }
