module DeepDescriptor.MCMC.Perturb where

import qualified Control.Lens as CL
import qualified Data.Array.Repa as DAR
import qualified Data.Random.Normal as DRN
import qualified Control.Monad as CM

import DeepDescriptor.MSR

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
      d' = unDegrees d + 1.0 * delta
      d''
        | d' > 60.0 = 60.0
        | d' < 20.0 = 20.0
        | otherwise = d'
    return $ mkDegrees d''

instance Perturb Vector3D where
  perturb std v = do
    delta <- randomVector3D std
    return $ DAR.computeS $ v DAR.+^ delta

-- | Perturb the origin and update the target so they track together.
instance Perturb (Target, Origin) where
  perturb std (t, o) = do
    delta <- randomVector3D std
    let
      o' = DAR.computeS $ (unOrigin o) DAR.+^ delta
      t' = DAR.computeS $ unTarget t DAR.+^ delta
    return (Target t', Origin o')

dot :: Vector3D -> Vector3D -> Double
dot v0 v1 = DAR.sumAllS $ v0 DAR.*^ v1

-- | Perturb the target and update the up vector.
-- The origin vector is used for reference but is not updated.
instance Perturb ((Origin, Up), Target) where
  perturb std ((o, u), t) = do
    let
      offset :: Vector3D
      offset = makeUnitLength $ DAR.computeS $ unTarget t DAR.-^ unOrigin o
    offset' <- CM.liftM makeUnitLength $ perturb std offset
    let
      t' = Target $ DAR.computeS $ offset' DAR.+^ unOrigin o
      uDotOffset'TimesOffset' = DAR.map (((unUp u) `dot` offset') *) offset'
      u' = Up $ makeUnitLength $ DAR.computeS $ unUp u DAR.-^ uDotOffset'TimesOffset'
    return ((o, u'), t')

-- | Perturb the up vector.
-- Neither the origin nor the target are updated.
instance Perturb ((Origin, Target), Up) where
  perturb std ((o, t), u) = do
    -- delta <- randomVector3D std
    let
      offset = makeUnitLength $ DAR.computeS $ unTarget t DAR.-^ unOrigin o
    u' <- CM.liftM makeUnitLength $ perturb std $ unUp u
    let
      -- t' = Target $ DAR.computeS $ offset' DAR.+^ o
      uDotOffsetTimesOffset = DAR.map ((u' `dot` offset) *) offset
      u'' = Up $ DAR.computeS $ u' DAR.-^ uDotOffsetTimesOffset
    return ((o, t), u'')

multiplierDegrees :: Double
multiplierDegrees = 0.5

multiplierOrigin :: Double
multiplierOrigin = 10.0

multiplierTarget :: Double
multiplierTarget = 0.5

multiplierUp :: Double
multiplierUp = 0.5

instance Perturb CameraFrame where
  perturb std cf = do
    let
      o = cf CL.^. origin
      t = cf CL.^. target
      u = cf CL.^. up
    f <- perturb (multiplierDegrees * std) $ cf CL.^. fovInDegrees
    (t', o') <- perturb (multiplierOrigin * std) (t, o)
    ((o'', u''), t'') <- perturb (multiplierTarget * std) ((o', u), t')
    ((o''', t'''), u''') <- perturb (multiplierUp * std) ((o'', t''), u'')
    return $ mkCameraFrame f o''' t''' u'''

instance Perturb Sensor where
  perturb std s = do
    cf' <- perturb std $ s CL.^. cameraFrame
    return $ CL.set cameraFrame cf' s

instance Perturb View where
  perturb std v = do
    s' <- perturb std $ v CL.^. sensor
    return $ CL.set sensor s' v
