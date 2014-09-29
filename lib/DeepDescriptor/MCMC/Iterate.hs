module DeepDescriptor.MCMC.Iterate where

-- import Data.Random.Normal
-- import           System.FilePath.Posix
-- import Data.Array.Repa hiding (map)
-- import qualified Data.Array.Repa as R
-- import Control.Monad
-- import Control.Lens
-- import Text.Printf
-- import Control.Exception
-- import Control.Monad (unless)
-- import Pipes
-- import qualified Data.ByteString.Lazy.Char8 as ByteString
-- import qualified Data.ByteString.Char8 as ByteString
-- import Codec.Compression.GZip
-- import qualified Data.Sequence as Seq
-- import Data.Sequence ((<|))
-- import qualified Data.Foldable as Foldable
-- import System.IO.Error
import qualified Data.Foldable as DF
import qualified Control.Lens as CL
import qualified Control.Exception as CE
import qualified Text.Printf as TP
import qualified Codec.Picture as CP
import qualified Data.Array.Repa as DAR
import qualified GHC.Float as GF
import qualified System.FilePath.Posix as SFP
import qualified Data.Sequence as DS
import qualified Pipes as P
import qualified Control.Monad as CM
import qualified Formatting as F

import DeepDescriptor.Mitsuba.Render
import DeepDescriptor.MCMC.Score
import DeepDescriptor.MCMC.Perturb
import DeepDescriptor.MSR
import DeepDescriptor.System

baseSTD :: Double
baseSTD = 1.0

fastView :: Sensor -> Sensor
fastView s = CL.set sampleCount 1 s

runUntilSuccess :: forall a. IO a -> IO a
runUntilSuccess f = do
  e <- CE.try f :: IO (Either CE.SomeException a)
  case e of
    Left _ -> runUntilSuccess f
    Right x -> return x

rgbToImage :: RGBImageValid -> CP.Image CP.PixelRGBF
rgbToImage rgb' =
  let
    justElseZero :: Maybe Double -> Double
    justElseZero Nothing = 0.0
    justElseZero (Just x) = x
    fromXY x y = CP.PixelRGBF
      (GF.double2Float $ justElseZero $ DAR.index
        rgb'
        (DAR.Z DAR.:. y DAR.:. x DAR.:. 0))
      (GF.double2Float $ justElseZero $ DAR.index
        rgb'
        (DAR.Z DAR.:. y DAR.:. x DAR.:. 1))
      (GF.double2Float $ justElseZero $ DAR.index
        rgb'
        (DAR.Z DAR.:. y DAR.:. x DAR.:. 2))
    DAR.Z DAR.:. width DAR.:. height DAR.:. 3 = DAR.extent rgb'
  in
    CP.generateImage fromXY width height

showRendering :: RenderingValid -> String -> IO ()
showRendering r pattern = do
  let
    rgb' :: String
    rgb' = TP.printf pattern ("rgb" :: String)
  putStrLn $ TP.printf "Writing %s" rgb'
  CP.savePngImage rgb' $ CP.ImageRGBF $ rgbToImage $ r CL.^. rgbValid

getGoodRendering :: Model -> Sensor -> IO (Sensor, RenderingValid)
getGoodRendering m s = do
  s' <- perturb baseSTD s
  r <- CM.liftM mkRenderingValid $ runUntilSuccess $ render m $ fastView s'
  let
    q = quality r
  putStrLn $ TP.printf "Quality was %f" q
  rs <- randomString 8
  showRendering r $ TP.printf "/tmp/debug_rendering_%s_%s.png" rs ("%s" :: String)
  if q > 0.5
  then do
    putStrLn "Got a good rendering."
    r' <- CM.liftM mkRenderingValid $ runUntilSuccess $ render m s'
    return (s', r')
  else do
    putStrLn "Bad rendering, retrying."
    getGoodRendering m s

saveMSR ::FilePath ->  MSR -> IO ()
saveMSR outputRoot (MSR m v r) = do
  rs <- randomString 8
  showRendering r $ SFP.joinPath [
    renderingRoot outputRoot,
    TP.printf "rendering_%s_%s.png" rs ("%s" :: String)]
  writeCompressed
    (SFP.joinPath [mvrRoot outputRoot, TP.printf "mvc_%s.hss" rs])
    (show $ MSR m v r)

mcmc :: Model -> Sensor -> DS.Seq RenderingValid -> P.Producer MSR IO ()
mcmc !m !s !otherRenderings = do
   P.lift $ putStrLn $ TP.printf "Number of generated renderings: %d" $ DS.length otherRenderings
   (v', r') <- P.lift $ getGoodRendering m s
   (v'', r'') <- P.lift $ getGoodRendering m s
   let
     (vBetter, rBetter) =
       if score (DF.toList otherRenderings) r' >= score (DF.toList otherRenderings) r''
       then (v', r')
       else (v'', r'')
   P.yield $! MSR m vBetter rBetter
   mcmc m vBetter $ rBetter DS.<| otherRenderings
