module MCMC where

import RenderUtil
import Data.Random.Normal
import           System.FilePath.Posix
import Data.Array.Repa hiding (map)
import qualified Data.Array.Repa as R
import Control.Monad
import Control.Lens
import Score
import Text.Printf
import SystemUtil
import Control.Exception
import Control.Monad (unless)
import Pipes
-- import System.IO.Error

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

fastView :: View -> View
fastView v = set (sensorL . sampleCountL) 1 v

runUntilSuccess :: forall a. IO a -> IO a
runUntilSuccess f = do
  e <- try f :: IO (Either SomeException a)
  case e of
    Left _ -> runUntilSuccess f
    Right x -> return x

getGoodRendering :: Model -> View -> IO (View, Rendering)
getGoodRendering m v = do
  v' <- perturbView v
  -- v' <- return v
  r <- runUntilSuccess $ render m $ fastView v'
  let
    -- s = score [] r
    q = quality r
  -- putStrLn $ printf "Score was %f" s
  putStrLn $ printf "Quality was %f" $ quality r
  rs <- randomString 8
  showRendering r $ printf "/tmp/debug_rendering_%s_%s.png" rs "%s"
  if q > 0.5
  then do
    putStrLn "Got a good rendering."
    r' <- runUntilSuccess $ render m v'
    return (v', r')
  else do
    putStrLn "Bad rendering, retrying."
    getGoodRendering m v

data MVR = MVR Model View Rendering deriving (Show, Read)

outputRoot :: FilePath
outputRoot = "/home/eric/Downloads/mcmc"

renderingRoot :: FilePath
renderingRoot = joinPath [outputRoot, "rendering"]

mvrRoot :: FilePath
mvrRoot = joinPath [outputRoot, "mvr"]

saveMVR :: MVR -> IO ()
saveMVR (MVR m v r) = do
  rs <- randomString 8
  showRendering r $ joinPath [renderingRoot, printf "rendering_%s_%s.png" rs "%s"]
  writeFile (joinPath [mvrRoot, printf "mvc_%s.hss" rs]) $ show $ MVR m v r

mcmc2 :: Model -> View -> [Rendering] -> Producer MVR IO ()
mcmc2 !m !v !otherRenderings = do
   lift $ putStrLn $ printf "Number of generated renderings: %d" $ length otherRenderings
   (v', r') <- lift $ getGoodRendering m v
   (v'', r'') <- lift $ getGoodRendering m v
   let
     (vBetter, rBetter) =
       if score otherRenderings r' >= score otherRenderings r''
       then (v', r')
       else (v'', r'')
   yield $! MVR m vBetter rBetter
   mcmc2 m vBetter $ rBetter : otherRenderings

mcmc :: Model -> View -> [Rendering] -> IO ()
mcmc m v otherRenderings = do
   putStrLn $ printf "Number of generated renderings: %d" $ length otherRenderings
   (v', r') <- getGoodRendering m v
   (v'', r'') <- getGoodRendering m v
   let
     (vBetter, rBetter) =
       if score otherRenderings r' >= score otherRenderings r''
       then (v', r')
       else (v'', r'')
   rs <- randomString 8
   showRendering rBetter $ printf "/tmp/mcmc/rendering/rendering_%s_%s.png" rs "%s"
   writeFile (printf "/tmp/mcmc/mvc/mvc_%s.hss" rs) $ show $ MVR m vBetter rBetter
   mcmc m vBetter $ rBetter : otherRenderings
