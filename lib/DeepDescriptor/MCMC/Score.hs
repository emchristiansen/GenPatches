module DeepDescriptor.MCMC.Score (
  quality,
  score,
  ) where

import qualified Control.Lens as CL
import qualified Data.Array.Repa as DAR

import DeepDescriptor.MVR

quality :: RenderingValid -> Double
quality r =
  let
    d = r CL.^. depthValid
    maybeToInt Nothing = 0
    maybeToInt (Just _) = 1
    numForeground :: Int
    numForeground = DAR.sumAllS $ DAR.map maybeToInt d
  in
    fromIntegral numForeground / fromIntegral (DAR.size $ DAR.extent $ d)

uniqueness :: [RenderingValid] -> RenderingValid -> Double
uniqueness otherRenderings r =
  let
    distance :: RenderingValid -> Double
    distance r' =
      let
        justElseZero :: Maybe Double -> Double
        justElseZero Nothing = 0.0
        justElseZero (Just x) = x
        applyJEZ :: RenderingValid -> RGBImage
        applyJEZ array = DAR.computeS $ DAR.map justElseZero (array CL.^. rgbValid)
        difference :: DAR.Array DAR.D DAR.DIM3 Double
        difference = (applyJEZ r) DAR.-^ (applyJEZ r')
      in
        sqrt $ DAR.sumAllS $ DAR.map (\x -> x * x) difference
    distances = map distance otherRenderings
  in
    minimum $ 1.0 : distances

-- | score assigns a quality measure to a rendering in the context of other
-- renderings.
-- To get a high score, a rendering must have high quality (few junk pixels)
-- and be different from the other images in the collection.
score :: [RenderingValid] -> RenderingValid -> Double
score otherRenderings r = if quality r > 0.5
  then quality r * uniqueness otherRenderings r
  else 0.0
