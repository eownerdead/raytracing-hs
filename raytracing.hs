module Main where

import Control.Lens
import Linear.Metric
import Linear.V3
import Linear.Vector
import Relude


type Point = V3 Double -- Point of the viewport.


type Color = V3 Double -- (r, g, b)


data Ray = Ray {_orig :: Point, _dir :: V3 Double}


$(makeLenses ''Ray)


aspectRatio :: Double
aspectRatio = 16.0 / 9.0


-- Pixel size of a image.
ph, pw :: Int
ph = 480
pw = round $ fromIntegral ph * aspectRatio


-- Viewport size.
vh, vw :: Double
vh = 2.0
vw = vh * (fromIntegral pw / fromIntegral ph)


u, v :: Point
u = V3 vw 0 0 -- Vector from left edge to right edge.
v = V3 0 (-vh) 0 -- Vector from upper edge to lower edge.


-- Vector whose size equivalent to a pixel.
deltaU, deltaV :: Point
deltaU = u ^/ fromIntegral pw
deltaV = v ^/ fromIntegral ph


forcalLen :: Double
forcalLen = 1.0


camera :: Point
camera = zero


upperLeft :: Point
upperLeft = camera - V3 0.0 0.0 forcalLen - u / 2 - v / 2


-- Correspond to pixel (0, 0).
p00 :: Point
p00 = upperLeft + 0.5 * (deltaU + deltaV)


at :: Double -> Ray -> Point
at t x = (x ^. orig) + t *^ (x ^. dir)


rayColor :: Ray -> Color
rayColor r = (1.0 - a) *^ V3 1 1 1 + a *^ V3 0.5 0.7 1
  where
    a = 0.5 * ((signorm (r ^. dir) ^. _y) + 1.0)


toColor :: Color -> Text
toColor x = unwords $ fmap (\y -> show (floor $ 255.999 * (x ^. y) :: Int)) [_x, _y, _z]


main :: IO ()
main = do
  putTextLn "P3"
  putTextLn $ show pw <> " " <> show ph
  putTextLn "255"
  forM_ ((,) <$> [0 .. (ph - 1)] <*> [0 .. (pw - 1)]) $ \(j, i) -> do
    let pixelCenter = p00 + (fromIntegral i *^ deltaU) + (fromIntegral j *^ deltaV)
    let rayDir = pixelCenter - camera
    putTextLn $ toColor $ rayColor $ Ray camera rayDir
