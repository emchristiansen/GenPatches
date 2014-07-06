module MCMC where

import RenderUtil
import Data.Random.Normal
import Data.Array.Repa hiding (map)
import Control.Monad
import Control.Lens

perturb :: Sensor -> IO Sensor
perturb s = do
  let
    pFOV :: Double -> IO Double
    pFOV fov = do
      delta <- normalIO
      if delta > 60.0
        then return 60.0
      else if delta < 20.0
        then return 20.0
      else return delta
    pVector :: Double -> Array U DIM1 Double -> IO (Array U DIM1 Double)
    pVector std vector = do
      delta <- liftM (map (* std)) $ liftM (take 3) $ normalsIO
      let
        d = fromListUnboxed (Z :. 3) delta
      return $ computeS $ vector +^ d
    pTarget :: Array U DIM1 Double -> IO (Array U DIM1 Double)
    pTarget target = do
      p <- pVector 0.1 $ computeS $ target -^ (s ^. originL)
      return $ computeS $ (s ^. originL) +^ p
  pFOV' <- pFOV $ s ^. fovInDegreesL
  pOrigin <- pVector 10.0 $ s ^. originL
  pTarget' <- pTarget $ s ^. targetL
  return undefined
