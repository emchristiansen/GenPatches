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
import DeepDescriptor.MVR
import DeepDescriptor.System

baseSTD :: Double
baseSTD = 1.0

fastView :: View -> View
fastView v = CL.set (sensor . sampleCount) 1 v

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
      (GF.double2Float $ justElseZero $ DAR.index rgb' (DAR.Z DAR.:. y DAR.:. x DAR.:. 0))
      (GF.double2Float $ justElseZero $ DAR.index rgb' (DAR.Z DAR.:. y DAR.:. x DAR.:. 1))
      (GF.double2Float $ justElseZero $ DAR.index rgb' (DAR.Z DAR.:. y DAR.:. x DAR.:. 2))
    DAR.Z DAR.:. width DAR.:. height DAR.:. 3 = DAR.extent rgb'
  in
    CP.generateImage fromXY width height

showRendering :: RenderingValid -> String -> IO ()
showRendering r pattern = do
  let
    rgb' :: String
    rgb' = TP.printf pattern ("rgb" :: String)
    -- distance = printf pattern "distance"
  putStrLn $ TP.printf "Writing %s" rgb'
  CP.savePngImage rgb' $ CP.ImageRGBF $ rgbToImage $ r CL.^. rgbValid
  -- putStrLn $ printf "Writing %s" distance
  -- savePngImage distance $ ImageYF $ distanceToImage $
    -- r ^. distanceL

-- getGoodRendering :: Model -> (Integrator -> View) -> IO (View, RenderingValid)
-- getGoodRendering m v = do
--   -- v' <- return $ \i -> perturb baseSTD (v i)
--   r <- runUntilSuccess $ render m (\i -> fastView v')
--   let
--     q = quality r
--   putStrLn $ TP.printf "Quality was %f" q
--   rs <- randomString 8
--   showRendering r $ TP.printf "/tmp/debug_rendering_%s_%s.png" rs "%s"
--   if q > 0.5
--   then do
--     putStrLn "Got a good rendering."
--     r' <- runUntilSuccess $ render m v'
--     return (v', r')
--   else do
--     putStrLn "Bad rendering, retrying."
--     getGoodRendering m v

-- saveMVR ::FilePath ->  MVR -> IO ()
-- saveMVR outputRoot (MVR m v r) = do
--   rs <- randomString 8
--   showRendering r $ SFP.joinPath [
--     renderingRoot outputRoot,
--     TP.printf "rendering_%s_%s.png" rs "%s"]
--   writeCompressed
--     (SFP.joinPath [mvrRoot outputRoot, TP.printf "mvc_%s.hss" rs])
--     (show $ MVR m v r)

-- mcmc :: Model -> View -> DS.Seq Rendering -> P.Producer MVR IO ()
-- mcmc !m !v !otherRenderings = do
--    P.lift $ putStrLn $ TP.printf "Number of generated renderings: %d" $ DS.length otherRenderings
--    (v', r') <- P.lift $ getGoodRendering m v
--    (v'', r'') <- P.lift $ getGoodRendering m v
--    let
--      (vBetter, rBetter) =
--        if score (DF.toList otherRenderings) r' >= score (DF.toList otherRenderings) r''
--        then (v', r')
--        else (v'', r'')
--    P.yield $! MVR m vBetter rBetter
--    mcmc m vBetter $ rBetter DS.<| otherRenderings
