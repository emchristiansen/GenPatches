module MCMC where

import RenderUtil
import Data.Random.Normal
import Data.Array.Repa hiding (map)
import qualified Data.Array.Repa as R
import Control.Monad
import Control.Lens
import Score
import Text.Printf
import SystemUtil

makeUnitLength :: Array U DIM1 Double -> Array U DIM1 Double
makeUnitLength vector =
  let
    magnitude = sqrt $ sumAllS $ R.map (\x -> x * x) vector
  in
    computeS $ R.map (/ magnitude) vector

perturb :: Sensor -> IO Sensor
perturb s = do
  let
    pFOV :: Double -> IO Double
    pFOV fov = do
      delta <- normalIO
      let fov' = fov + delta
      return $ if fov' > 60.0
        then 60.0
      else if fov' < 20.0
        then 20.0
      else fov'
    pVector :: Double -> Array U DIM1 Double -> IO (Array U DIM1 Double)
    pVector std vector = do
      delta <- liftM (map (* std) . take 3) normalsIO
      let
        d = fromListUnboxed (Z :. 3) delta
      return $ computeS $ vector +^ d
    pTarget :: Array U DIM1 Double -> IO (Array U DIM1 Double)
    pTarget target = do
      p <- pVector 0.1 $ computeS $ target -^ (s ^. originL)
      return $ computeS $ s ^. originL +^ makeUnitLength p
  pFOV' <- pFOV $ s ^. fovInDegreesL
  pOrigin <- pVector 10.0 $ s ^. originL
  pTarget' <- pTarget $ s ^. targetL
  let
    mutate :: Sensor -> Sensor
    mutate = set fovInDegreesL pFOV' . set originL pOrigin . set targetL pTarget'
  return $ mutate s

perturbView :: View -> IO View
perturbView v = do
  s' <- perturb $ v ^. sensorL
  return $ set sensorL s' v

getGoodRendering :: Model -> View -> IO (View, Rendering)
getGoodRendering m v = do
  v' <- perturbView v
  r <- render m v'
  let
    s = score [] r
  if s > 0.6
  then do
    print "Got a good rendering."
    return (v', r)
  else do
    print "Bad rendering, retrying."
    getGoodRendering m v'

mcmc :: Model -> View -> IO ()
mcmc m v = do
   (v', r) <- getGoodRendering m v
   rs <- randomString 8
   showRendering r $ printf "/tmp/mcmc_rendering_%s.png" rs
   mcmc m v'
