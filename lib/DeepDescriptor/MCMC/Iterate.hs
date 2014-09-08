module DeepDescriptor.MCMC.Iterate where

import Data.Random.Normal
import           System.FilePath.Posix
import Data.Array.Repa hiding (map)
import qualified Data.Array.Repa as R
import Control.Monad
import Control.Lens
import Text.Printf
import Control.Exception
import Control.Monad (unless)
import Pipes
import qualified Data.ByteString.Lazy.Char8 as ByteString
-- import qualified Data.ByteString.Char8 as ByteString
import Codec.Compression.GZip
import qualified Data.Sequence as Seq
-- import Data.Sequence ((<|))
import qualified Data.Foldable as Foldable
-- import System.IO.Error

import DeepDescriptor.Render
import DeepDescriptor.Score
import DeepDescriptor.System


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

saveMVR :: MVR -> IO ()
saveMVR (MVR m v r) = do
  rs <- randomString 8
  showRendering r $ joinPath [renderingRoot, printf "rendering_%s_%s.png" rs "%s"]
  writeCompressed (joinPath [mvrRoot, printf "mvc_%s.hss" rs]) $ show $ MVR m v r

mcmc2 :: Model -> View -> Seq.Seq Rendering -> Producer MVR IO ()
mcmc2 !m !v !otherRenderings = do
   lift $ putStrLn $ printf "Number of generated renderings: %d" $ Seq.length otherRenderings
   (v', r') <- lift $ getGoodRendering m v
   (v'', r'') <- lift $ getGoodRendering m v
   let
     (vBetter, rBetter) =
       if score (Foldable.toList otherRenderings) r' >= score (Foldable.toList otherRenderings) r''
       then (v', r')
       else (v'', r'')
   yield $! MVR m vBetter rBetter
   mcmc2 m vBetter $ rBetter Seq.<| otherRenderings

