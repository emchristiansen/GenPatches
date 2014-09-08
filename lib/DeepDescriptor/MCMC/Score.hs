module DeepDescriptor.MCMC.Score where

import           Data.Array.Repa       hiding (extract, map, (++))
import qualified Data.Array.Repa as R
import Control.Lens

import DeepDescriptor.Render

getMask :: Rendering -> Array U DIM3 Bool
getMask r = computeS $ R.map (\d -> d >= 1 && d <= 2800) $ r ^. distanceL

quality :: Rendering -> Double
quality r =
  let
    mask = getMask r
    boolToInt False = 0
    boolToInt True = 1
    numForeground :: Int
    numForeground = sumAllS $ R.map boolToInt mask
  in
    fromIntegral numForeground / fromIntegral (size $ extent mask)

uniqueness :: [Rendering] -> Rendering -> Double
uniqueness otherRenderings r =
  let
    distance :: Rendering -> Double
    distance r' =
      let
        difference :: Array D DIM3 Double
        difference = (r ^. rgbL) -^ (r' ^. rgbL)
      in
        sqrt $ sumAllS $ R.map (\x -> x * x) difference
    distances = map distance otherRenderings
  in
    minimum $ 1.0 : distances

score :: [Rendering] -> Rendering -> Double
score otherRenderings r = if quality r < 0.5
  then 0.0
  else quality r * uniqueness otherRenderings r
