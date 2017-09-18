{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
import GHC.Generics
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Maybe
import qualified Data.ByteString.Lazy as BS

data Pixel = Pixel
  { point :: [Float]
  , strip :: Int
  , offset :: Int
  }
  deriving (Generic, ToJSON, FromJSON, Show)

type Strip = [Maybe Pixel]

skipPixels :: Int -> Strip
skipPixels 0 = []
skipPixels count = Nothing : (skipPixels (count - 1))

heightPerLevel = 1.3

pointForTheta :: Float -> Float -> Float -> [Float]
pointForTheta theta radius height =
  [radius * cos theta, radius * sin theta, height]

pointsOnCircle :: Int -> Float -> Float -> [[Float]]
pointsOnCircle pointCount radius height = let
  gap :: Float
  gap = 2 * pi / (fromIntegral pointCount)
  thetas :: [Float]
  thetas = take pointCount $ iterate (+ gap) 0
  in
    map (\t -> pointForTheta t radius height) thetas

makePixels :: [[Float]] -> Int -> Int -> [Pixel]
makePixels [] _ _ = []
makePixels points strip baseOffset = let
  offsets = [baseOffset..]
  rest = makePixels (tail points) strip (1 + baseOffset)
  in
    Pixel {point = head points, strip = strip, offset = baseOffset} : rest

makeStrip :: Int -> Float -> Float -> Int -> Int -> Strip
makeStrip pointCount radius stripGap stripNumber baseOffset = let
  points = pointsOnCircle pointCount radius (stripGap * fromIntegral stripNumber)
  pixels = makePixels points stripNumber baseOffset
  in
    map Just pixels

pixels = let
  pixelPerStrip = 15
  radius = 15.87
  stripGap = 2.3
  strip stripNumber = makeStrip pixelPerStrip radius stripGap stripNumber 0
  in
    skipPixels 64
    ++ strip 0
    ++ strip 1
    ++ strip 2
    ++ strip 3
    ++ skipPixels 4
    ++ strip 4
    ++ strip 5
    ++ strip 6
    ++ strip 7
    ++ skipPixels 4
    ++ strip 8
    ++ strip 9

main = do
  BS.putStr $ encodePretty pixels
