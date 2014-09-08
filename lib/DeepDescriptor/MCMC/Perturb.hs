module DeepDescriptor.MCMC.Perturb where

makeUnitLength :: Array U DIM1 Double -> Array U DIM1 Double
makeUnitLength vector =
  let
    magnitude = sqrt $ sumAllS $ R.map (\x -> x * x) vector
  in
    computeS $ R.map (/ magnitude) vector

perturbFOV :: Sensor -> IO Sensor
perturbFOV s = do
  delta <- normalIO
  let
    fov' = (s ^. fovInDegreesL) + 0.1 * delta
    fov''
      | fov' > 60.0 = 60.0
      | fov' < 20.0 = 20.0
      | otherwise = fov'
  return $ set fovInDegreesL fov'' s

pVector :: Double -> Array U DIM1 Double -> IO (Array U DIM1 Double)
pVector std vector = do
  delta <- liftM (map (* std) . take 3) normalsIO
  let
    d = fromListUnboxed (Z :. 3) delta
  return $ computeS $ vector +^ d

perturbOrigin :: Sensor -> IO Sensor
perturbOrigin s = do
  let
    std = 10.0
  delta <- liftM (fromListUnboxed (Z :. 3)) $ liftM (map (* std) . take 3) normalsIO
  let
    origin' = computeS $ s ^. originL +^ delta
    target' = computeS $ s ^. targetL +^ delta
  return $ set originL origin' $ set targetL target' s

perturbTarget :: Sensor -> IO Sensor
perturbTarget s = do
  let
    offset = makeUnitLength $ computeS $ s ^. targetL -^ s ^. originL
  offset' <- liftM makeUnitLength $ pVector 0.1 offset
  let
    target' = computeS $ s ^. originL +^ offset'
  return $ set targetL target' s
  -- p <- pVector 0.1 $ computeS $ target -^ (s ^. originL)
  -- return $ computeS $ s ^. originL +^ makeUnitLength p

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
