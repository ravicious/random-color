module Color exposing
    ( Color
    , rgb, rgba, hsl, hsla, greyscale, grayscale, complement
    , Gradient, linear, radial
    , toRgb, toHsl
    , contrast, luminance, toHex, toRgbInt
    )

{-| Library for working with colors. Includes
[RGB](https://en.wikipedia.org/wiki/RGB_color_model) and
[HSL](http://en.wikipedia.org/wiki/HSL_and_HSV) creation, gradients, and
built-in names.


# Colors

@docs Color


# Creation

@docs rgb, rgba, hsl, hsla, greyscale, grayscale, complement


# Gradients

@docs Gradient, linear, radial


# Extracting Colors

@docs toRgb, toHsl

-}

import Basics exposing (..)
import Hex


{-| Representation of colors.
-}
type Color
    = RGBA Int Int Int Float
    | HSLA Float Float Float Float


{-| Create RGB colors with an alpha component for transparency.
The alpha component is specified with numbers between 0 and 1.
-}
rgba : Int -> Int -> Int -> Float -> Color
rgba =
    RGBA


{-| Create RGB colors from numbers between 0 and 255 inclusive.
-}
rgb : Int -> Int -> Int -> Color
rgb r g b =
    RGBA r g b 1


{-| Create [HSL colors](http://en.wikipedia.org/wiki/HSL_and_HSV)
with an alpha component for transparency.
-}
hsla : Float -> Float -> Float -> Float -> Color
hsla hue saturation lightness alpha =
    HSLA (hue - turns (toFloat (floor (hue / (2 * pi))))) saturation lightness alpha


{-| Create [HSL colors](http://en.wikipedia.org/wiki/HSL_and_HSV). This gives
you access to colors more like a color wheel, where all hues are arranged in a
circle that you specify with standard Elm angles (radians).

    red =
        hsl (degrees 0) 1 0.5

    green =
        hsl (degrees 120) 1 0.5

    blue =
        hsl (degrees 240) 1 0.5

    pastelRed =
        hsl (degrees 0) 0.7 0.7

To cycle through all colors, just cycle through degrees. The saturation level
is how vibrant the color is, like a dial between grey and bright colors. The
lightness level is a dial between white and black.

-}
hsl : Float -> Float -> Float -> Color
hsl hue saturation lightness =
    hsla hue saturation lightness 1


{-| Produce a gray based on the input. 0 is white, 1 is black.
-}
grayscale : Float -> Color
grayscale p =
    HSLA 0 0 (1 - p) 1


{-| Produce a gray based on the input. 0 is white, 1 is black.
-}
greyscale : Float -> Color
greyscale p =
    HSLA 0 0 (1 - p) 1


{-| Produce a &ldquo;complementary color&rdquo;. The two colors will
accent each other. This is the same as rotating the hue by 180&deg;.
-}
complement : Color -> Color
complement color =
    case color of
        HSLA h s l a ->
            hsla (h + degrees 180) s l a

        RGBA r g b a ->
            let
                ( h, s, l ) =
                    rgbToHsl r g b
            in
            hsla (h + degrees 180) s l a


{-| Extract the components of a color in the HSL format.
-}
toHsl : Color -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
toHsl color =
    case color of
        HSLA h s l a ->
            { hue = h, saturation = s, lightness = l, alpha = a }

        RGBA r g b a ->
            let
                ( h, s, l ) =
                    rgbToHsl r g b
            in
            { hue = h, saturation = s, lightness = l, alpha = a }


{-| Extract the components of a color in the RGB format.
-}
toRgb : Color -> { red : Int, green : Int, blue : Int, alpha : Float }
toRgb color =
    case color of
        RGBA r g b a ->
            { red = r, green = g, blue = b, alpha = a }

        HSLA h s l a ->
            let
                ( r, g, b ) =
                    hslToRgb h s l
            in
            { red = round (255 * r)
            , green = round (255 * g)
            , blue = round (255 * b)
            , alpha = a
            }


fmod : Float -> Int -> Float
fmod f n =
    let
        integer =
            floor f
    in
    toFloat (modBy n integer) + f - toFloat integer


rgbToHsl : Int -> Int -> Int -> ( Float, Float, Float )
rgbToHsl red green blue =
    let
        r =
            toFloat red / 255

        g =
            toFloat green / 255

        b =
            toFloat blue / 255

        cMax =
            max (max r g) b

        cMin =
            min (min r g) b

        c =
            cMax - cMin

        hue =
            degrees 60
                * (if cMax == r then
                    fmod ((g - b) / c) 6

                   else if cMax == g then
                    ((b - r) / c) + 2

                   else
                    {- cMax == b -}
                    ((r - g) / c) + 4
                  )

        lightness =
            (cMax + cMin) / 2

        saturation =
            if lightness == 0 then
                0

            else
                c / (1 - abs (2 * lightness - 1))
    in
    ( hue, saturation, lightness )


hslToRgb : Float -> Float -> Float -> ( Float, Float, Float )
hslToRgb hue saturation lightness =
    let
        chroma =
            (1 - abs (2 * lightness - 1)) * saturation

        normHue =
            hue / degrees 60

        x =
            chroma * (1 - abs (fmod normHue 2 - 1))

        ( r, g, b ) =
            if normHue < 0 then
                ( 0, 0, 0 )

            else if normHue < 1 then
                ( chroma, x, 0 )

            else if normHue < 2 then
                ( x, chroma, 0 )

            else if normHue < 3 then
                ( 0, chroma, x )

            else if normHue < 4 then
                ( 0, x, chroma )

            else if normHue < 5 then
                ( x, 0, chroma )

            else if normHue < 6 then
                ( chroma, 0, x )

            else
                ( 0, 0, 0 )

        m =
            lightness - chroma / 2
    in
    ( r + m, g + m, b + m )



--toV3 : Color -> V3
--toV4 : Color -> V4


{-| Abstract representation of a color gradient.
-}
type Gradient
    = Linear ( Float, Float ) ( Float, Float ) (List ( Float, Color ))
    | Radial ( Float, Float ) Float ( Float, Float ) Float (List ( Float, Color ))


{-| Create a linear gradient. Takes a start and end point and then a series of
&ldquo;color stops&rdquo; that indicate how to interpolate between the start and
end points. See [this example](http://elm-lang.org/examples/linear-gradient) for a
more visual explanation.
-}
linear : ( Float, Float ) -> ( Float, Float ) -> List ( Float, Color ) -> Gradient
linear =
    Linear


{-| Create a radial gradient. First takes a start point and inner radius. Then
takes an end point and outer radius. It then takes a series of &ldquo;color
stops&rdquo; that indicate how to interpolate between the inner and outer
circles. See [this example](http://elm-lang.org/examples/radial-gradient) for a
more visual explanation.
-}
radial : ( Float, Float ) -> Float -> ( Float, Float ) -> Float -> List ( Float, Color ) -> Gradient
radial =
    Radial


toHex : Color -> String
toHex color =
    let
        intToHex =
            Hex.toString >> String.padLeft 2 '0'

        { red, green, blue } =
            toRgb color
    in
    String.concat [ intToHex red, intToHex green, intToHex blue ]


{-| Based on <https://stackoverflow.com/a/9733420/742872>
-}
luminance : Color -> Float
luminance color =
    let
        { red, green, blue } =
            toRgb color

        calculateV =
            \x ->
                let
                    v =
                        toFloat x / 255
                in
                if v <= 0.03928 then
                    v / 12.92

                else
                    ((v + 0.055) / 1.055) ^ 2.4
    in
    calculateV red * 0.2126 + calculateV green * 0.7152 + calculateV blue * 0.0722


contrast : Color -> Color -> Float
contrast color1 color2 =
    let
        result =
            (luminance color1 + 0.05) / (luminance color2 + 0.05)
    in
    if result < 1 then
        1 / result

    else
        result


toRgbInt : Color -> Int
toRgbInt color =
    let
        { red, green, blue } =
            toRgb color
    in
    red * 1000000 + green * 1000 + blue
