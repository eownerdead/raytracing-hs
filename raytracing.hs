module Main where

import Control.Lens
import Data.Text.IO qualified as T
import Linear.Metric
import Linear.V3
import Linear.Vector
import Relude
import System.Random.Stateful


type Point = V3 Double -- Point of the viewport.


newtype Color = Color (V3 Double) -- (r, g, b)
  deriving (Num, Fractional)


type Time = Double


data Interval = Interval {_inf :: Double, _sup :: Double}


$(makeLenses ''Interval)


len :: Interval -> Double
len i = (i ^. sup) - (i ^. inf)


contains :: Double -> Interval -> Bool
contains x i = (i ^. inf) <= x && x <= (i ^. sup)


surrounds :: Double -> Interval -> Bool
surrounds x i = (i ^. inf) < x && x < (i ^. sup)


clamp :: Double -> Interval -> Double
clamp x i
  | x < i ^. inf = i ^. inf
  | i ^. sup < x = i ^. sup
  | otherwise = x


data Ray = Ray {_orig :: Point, _dir :: V3 Double}


$(makeLenses ''Ray)


data Hit = Hit
  {_pHit :: Point, _normal :: V3 Double, _tHit :: Time, _frontFace :: Bool}


$(makeLenses ''Hit)


class Hittable a where
  hit :: Interval -> a -> Ray -> Maybe Hit


data Sphere = Sphere {_center :: Point, _radius :: Double}


$(makeLenses ''Sphere)


instance Hittable Sphere where
  hit i x r
    | d < 0 = Nothing
    | surrounds t1 i = hit' t1
    | surrounds t2 i = hit' t2
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

      hit' :: Time -> Maybe Hit
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
  hit _ [] _ = Nothing
  hit i (x : xs) r
    | Just hx <- hit (i & sup .~ maybe (i ^. sup) (^. tHit) h) xs r = Just hx
    | otherwise = h
    where
      h = hit i x r


world :: [Sphere]
world = [Sphere (V3 0 0 (-1)) 0.5, Sphere (V3 0 (-100.5) (-1)) 100]


aspectRatio :: Double
aspectRatio = 16.0 / 9.0


-- Pixel size of a image.
ph, pw :: Int
ph = round $ fromIntegral pw / aspectRatio
pw = 400


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


samplesPerPixel :: Int
samplesPerPixel = 100


maxDepth :: Int
maxDepth = 50


gamma :: Double
gamma = 2


rayAt :: Time -> Ray -> Point
rayAt t x = (x ^. orig) + t *^ (x ^. dir)


rayColor :: (Hittable a, StatefulGen g m) => Int -> a -> Ray -> g -> m Color
rayColor depth xs r g
  | depth <= 0 = pure $ Color $ V3 0 0 0
  | Just x <- hit (Interval 0.001 infinity) xs r = do
      s <- randUnit g
      (0.1 *) <$> rayColor (depth - 1) xs (Ray (x ^. pHit) ((x ^. normal) + s)) g
  | otherwise = pure $ Color $ (1.0 - a) *^ V3 1 1 1 + a *^ V3 0.5 0.7 1
  where
    a = 0.5 * ((signorm (r ^. dir) ^. _y) + 1.0)


toColor :: Color -> Text
toColor (Color x) =
  unwords
    $ toList x
    <&> (\i -> show (floor $ 256 * clamp (i ** (1 / gamma)) intensity :: Int))
  where
    intensity = Interval{_inf = 0, _sup = 0.999}


getRay :: StatefulGen g m => Int -> Int -> g -> m Ray
getRay i j g = do
  x <- uniformRM (-0.5, 0.5) g
  y <- uniformRM (-0.5, 0.5) g
  let sample = p00 + ((fromIntegral i + x) *^ deltaU) + ((fromIntegral j + y) *^ deltaV)
  pure $ Ray camera (sample - camera)


-- rejection method
randUnit :: StatefulGen g m => g -> m (V3 Double)
randUnit g = do
  x <- uniformRM (0, 1) g
  y <- uniformRM (0, 1) g
  z <- uniformRM (0, 1) g
  let a = V3 x y z
  if norm a > 1
    then randUnit g
    else pure $ signorm a


main :: IO ()
main = runStateGenT_ (mkStdGen 0) $ \g -> do
  putTextLn "P3"
  putTextLn $ show pw <> " " <> show ph
  putTextLn "255"
  forM_ [0 .. (ph - 1)] $ \j -> do
    liftIO $ T.hPutStrLn stderr $ "Scanlines remaining: " <> show (ph - j)
    forM_ [0 .. (pw - 1)] $ \i -> do
      color <-
        sum
          <$> replicateM samplesPerPixel (getRay i j g >>= \x -> rayColor maxDepth world x g)
      putTextLn $ toColor $ color / fromIntegral samplesPerPixel
