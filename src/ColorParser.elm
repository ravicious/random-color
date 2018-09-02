module ColorParser exposing (parse)

import Color exposing (Color)
import Hex
import Parser exposing ((|.), (|=), Parser, chompIf)


parse : String -> Result String Color
parse =
    Parser.run colorParser >> Result.mapError Parser.deadEndsToString


byteParser : Parser Int
byteParser =
    Parser.succeed ()
        |. chompIf Char.isHexDigit
        |. chompIf Char.isHexDigit
        |> Parser.getChompedString
        |> Parser.andThen
            (\hex ->
                case hex |> String.toLower |> Hex.fromString of
                    Ok int ->
                        Parser.succeed int

                    Err string ->
                        Parser.problem string
            )


colorParser : Parser Color
colorParser =
    Parser.succeed Color.rgb
        |= byteParser
        |= byteParser
        |= byteParser
        |. Parser.end
