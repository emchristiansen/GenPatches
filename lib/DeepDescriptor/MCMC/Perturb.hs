module DeepDescriptor.MCMC.Perturb where

import qualified Control.Lens as CL
import qualified Data.Array.Repa as DAR
import qualified Data.Random.Normal as DRN
import qualified Control.Monad as CM

import DeepDescriptor.MVR

class Perturb a where
  perturb :: Double -> a -> IO a

makeUnitLength :: Vector3D -> Vector3D
makeUnitLength vector =
  let
    magnitude = sqrt $ DAR.sumAllS $ DAR.map (\x -> x * x) vector
  in
    DAR.computeS $ DAR.map (/ magnitude) vector

randomVector3D :: Double -> IO Vector3D
randomVector3D std = do
  list <- CM.liftM (map (* std) . take 3) DRN.normalsIO
  return $ DAR.fromListUnboxed (DAR.Z DAR.:. 3) list

instance Perturb Degrees where
  perturb std d = do
    delta <- CM.liftM (std *) DRN.normalIO
    let
      d' = d + 1.0 * delta
      d''
        | d' > 60.0 = 60.0
        | d' < 20.0 = 20.0
        | otherwise = d'
    return d''

instance Perturb (Double, Vector3D) where
  perturb std v = do
    delta <- randomVector3D std
    return $ DAR.computeS $ v DAR.+^ delta

-- | Perturb the origin and update the target so they track together.
instance Perturb (Target, Origin) where
  perturb std (t, o) = do
    delta <- randomVector3D std
    let
      o' = DAR.computeS $ o DAR.+^ delta
      t' = DAR.computeS $ t DAR.+^ delta
    return (t', o')

dot :: Vector3D -> Vector3D -> Double
dot v0 v1 = DAR.sumAllS $ v0 DAR.*^ v1

-- | Perturb the target and update the up vector.
-- The origin vector is used for reference but is not updated.
instance Perturb ((Origin, Up), Target) where
  perturb std ((o, u), t) = do
    delta <- randomVector3D std
    let
      offset = makeUnitLength $ DAR.computeS $ unTarget t DAR.-^ unOrigin o
    offset' <- CM.liftM makeUnitLength $ perturb offset
    let
      t' = Target $ DAR.computeS $ offset' DAR.+^ o
      uDotOffset'TimesOffset' = DAR.map (((unUp u) `dot` offset') *) offset'
      u' = Up $ DAR.computeS $ unUp u DAR.-^ uDotOffset'TimesOffset'
    return ((o, u'), t')

perturbUp :: Sensor -> IO Sensor
perturbUp s = do
  let
    up = makeUnitLength $ s ^. upL
  up' <- liftM makeUnitLength $ pVector 0.1 up
  return $ set upL up' s

perturb :: Sensor -> IO Sensor
-- perturb s = perturbFOV s >>= perturbOrigin >>= perturbTarget
perturb s = perturbTarget s >>= perturbOrigin >>= perturbFOV >>= perturbUp
  -- let
    -- mutate :: Sensor -> Sensor
    -- mutate = set fovInDegreesL pFOV' . set originL pOrigin . set targetL pTarget'
  -- return $ mutate s

perturbView :: View -> IO View
perturbView v = do
  s' <- perturb $ v ^. sensorL
  return $ set sensorL s' v
