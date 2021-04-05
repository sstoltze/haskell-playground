module Colour where

import Codec.Picture (PixelRGB8(..))

data Colour = Colour { colourRed :: Int
                     , colourGreen :: Int
                     , colourBlue :: Int
                     } deriving Show

colourPixel :: Colour -> PixelRGB8
colourPixel (Colour r g b) = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)

createColour :: Int -> Int -> Int -> Colour
createColour r g b = Colour (clamp r) (clamp g) (clamp b)
  where
    clamp n = max (min n 255) 0

colourScale :: RealFrac a => a -> Colour -> Colour
colourScale s (Colour r g b) = createColour (scale r) (scale g) (scale b)
  where
    scale n = round (s * fromIntegral n)
