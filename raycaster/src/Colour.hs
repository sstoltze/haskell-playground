module Colour where

import Codec.Picture (PixelRGB8(..))

data Colour = ColourRGB { colourRed :: Int
                        , colourGreen :: Int
                        , colourBlue :: Int
                        }
            | ColourHSL { colourHue :: Double
                        , colourSaturation :: Double
                        , colourLuminance :: Double
                        }
            deriving Show

colourPixel :: Colour -> PixelRGB8
colourPixel c@(ColourHSL _ _ _) = colourPixel $ colourToRgb c
colourPixel (ColourRGB r g b) = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)

createColour :: Int -> Int -> Int -> Colour
createColour r g b = ColourRGB (clamp r) (clamp g) (clamp b)
  where
    clamp n = max (min n 255) 0

createColourHsl :: Double -> Double -> Double -> Colour
createColourHsl h s l = ColourHSL (fmod h 360) s l
  where
    fmod x n = x - n * (fromInteger $ floor $ x / n)

colourScale :: RealFrac a => a -> Colour -> Colour
colourScale s c@(ColourHSL _ _ _) = colourScale s $ colourToRgb c
colourScale s (ColourRGB r g b) = createColour (scale r) (scale g) (scale b)
  where
    scale n = round (s * fromIntegral n)

inverseColour :: Colour -> Colour
inverseColour c@(ColourHSL _ _ _) = inverseColour $ colourToRgb c
inverseColour (ColourRGB r g b) = createColour (255 - r) (255 - g) (255 - b)

colourToHsl :: Colour -> Colour
colourToHsl c@(ColourHSL _ _ _) = c
colourToHsl (ColourRGB r g b) = ColourHSL hue saturation lightness
  where
    hueMod x = x - 6 * (fromInteger $ floor $ x / 6)
    r' = (fromIntegral r) / 255
    g' = (fromIntegral g) / 255
    b' = (fromIntegral b) / 255
    xMax = maximum [r', g', b']
    xMin = minimum [r', g', b']
    chroma = xMax - xMin
    lightness = (xMax + xMin) / 2
    hue = 60 * if xMax == xMin then 0
      else if xMax == r' then hueMod $ (g' - b') / chroma
      else if xMax == g' then hueMod $ (b' - r') / chroma + 2
      else hueMod $ (r' - g') / chroma + 4
    saturation = if lightness == 0 || lightness == 1
      then 0
      else chroma/(1 - (abs $ 2 * lightness - 1))

colourToRgb :: Colour -> Colour
colourToRgb c@(ColourRGB _ _ _) = c
colourToRgb (ColourHSL h' s l) = createColour r g b
  where
    h = h' - 360 * (fromInteger $ floor $ h' / 360)
    hueMod a = a - 2 * (fromInteger $ floor $ a / 2)
    chroma = (1 - (abs $ 2 * l - 1)) * s
    x = chroma * (1 - (abs $ (hueMod $ h / 60) - 1))
    m = l - chroma/2
    (r',g',b') = if 0 <= h && h < 60 then (chroma, x, 0)
      else if 60 <= h && h < 120 then (x, chroma, 0)
      else if 120 <= h && h < 180 then (0, chroma, x)
      else if 180 <= h && h < 240 then (0, x, chroma)
      else if 240 <= h && h < 300 then (x, 0, chroma)
      else (chroma, 0, x)
    r = round $ 255 * (r' + m)
    g = round $ 255 * (g' + m)
    b = round $ 255 * (b' + m)

rotateColour :: Double -> Colour -> Colour
rotateColour angle c@(ColourRGB _ _ _) = rotateColour angle $ colourToHsl c
rotateColour angle (ColourHSL h s l) = createColourHsl (angle + h) s l
