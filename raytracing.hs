module Main where

import Control.Lens
import Linear.Metric
import Linear.V3
import Linear.Vector
import Relude


type Point = V3 Double -- Point of the viewport.


newtype Color = Color (V3 Double) -- (r, g, b)


data Ray = Ray {_orig :: Point, _dir :: V3 Double}


$(makeLenses ''Ray)


data Hit = Hit
  {_pHit :: Point, _normal :: V3 Double, _tHit :: Double, _frontFace :: Bool}


$(makeLenses ''Hit)


class Hittable a where
  hit :: Double -> Double -> a -> Ray -> Maybe Hit


data Sphere = Sphere {_center :: Point, _radius :: Double}


$(makeLenses ''Sphere)


instance Hittable Sphere where
  hit tmin tmax x r
    | d < 0 = Nothing
    | tmin < t1 && t1 < tmax = hit' t1
    | tmin < t2 && t2 < tmax = hit' t2
    | otherwise = Nothing
    where
      oc = (x ^. center) - (r ^. orig)

      -- Quadratic formula
      a = quadrance (r ^. dir)
      b' = (r ^. dir) `dot` oc
      c = quadrance oc - (x ^. radius) ^ (2 :: Int)
      d = b' ^ (2 :: Int) - a * c

      t1 = (b' - sqrt d) / a
      t2 = (b' + sqrt d) / a

      hit' :: Double -> Maybe Hit
      hit' t =
        Just
          $ Hit
            { _pHit = p
            , _tHit = t
            , _frontFace = ff
            , _normal = if ff then outwardNormal else -outwardNormal
            }
        where
          p = rayAt t r
          outwardNormal = signorm $ p - (x ^. center)
          ff = (r ^. dir) `dot` outwardNormal < 0


instance Hittable a => Hittable [a] where
  hit _ _ [] _ = Nothing
  hit tmin tmax (x : xs) r
    | Just hx <- hit tmin (maybe tmax (^. tHit) h) xs r = Just hx
    | otherwise = h
    where
      h = hit tmin tmax x r


world :: [Sphere]
world = [Sphere (V3 0 0 (-1)) 0.5, Sphere (V3 0 (-100.5) (-1)) 100]


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


infinity :: Double
infinity = 1 / 0


rayAt :: Double -> Ray -> Point
rayAt t x = (x ^. orig) + t *^ (x ^. dir)


rayColor :: Hittable a => a -> Ray -> Color
rayColor xs r
  | Just x <- hit 0 infinity xs r = Color $ 0.5 *^ ((x ^. normal) + V3 1 1 1)
  | otherwise = Color $ (1.0 - a) *^ V3 1 1 1 + a *^ V3 0.5 0.7 1
  where
    a = 0.5 * ((signorm (r ^. dir) ^. _y) + 1.0)


toColor :: Color -> Text
toColor (Color x) = unwords $ fmap (\y -> show (floor $ 255.999 * (x ^. y) :: Int)) [_x, _y, _z]


main :: IO ()
main = do
  putTextLn "P3"
  putTextLn $ show pw <> " " <> show ph
  putTextLn "255"
  forM_ ((,) <$> [0 .. (ph - 1)] <*> [0 .. (pw - 1)]) $ \(j, i) -> do
    let pixelCenter = p00 + (fromIntegral i *^ deltaU) + (fromIntegral j *^ deltaV)
    let rayDir = pixelCenter - camera
    putTextLn $ toColor $ rayColor world (Ray camera rayDir)
