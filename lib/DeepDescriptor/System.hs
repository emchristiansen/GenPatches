module DeepDescriptor.System where

import qualified Control.Monad as CM
import qualified System.Random as SR
import qualified Text.Printf as TP
import qualified Data.ByteString.Lazy.Char8 as DBLC
import qualified Codec.Compression.GZip as CCG
import qualified System.FilePath.Posix as SFP
import qualified Shelly as S
import qualified Data.Text as DT

runShell :: String -> IO()
runShell command = do
  putStrLn $ TP.printf "Running shell command: %s" command
  let
    binary : arguments = map DT.pack $ words command
  _ <- S.shelly $ S.silently $ S.run (S.fromText binary) arguments
  return ()

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory from to = runShell $ TP.printf "cp -r %s %s" from to

makeDirectory :: FilePath -> IO ()
makeDirectory dir = runShell $ TP.printf "mkdir -p %s" dir

randomString :: Int -> IO String
randomString length' = do
  let chars = ['a' .. 'z'] ++ ['0' .. '9']
  indices' <- CM.replicateM length' $ SR.randomRIO (0, length chars - 1)
  return $ map (chars !!) indices'

outputRoot :: FilePath
outputRoot = "/home/eric/Downloads/mcmc"

renderingRoot :: FilePath
renderingRoot = SFP.joinPath [outputRoot, "rendering"]

mvrRoot :: FilePath
mvrRoot = SFP.joinPath [outputRoot, "mvr"]

writeCompressed :: Show a => FilePath -> a -> IO ()
writeCompressed path contents =
  DBLC.writeFile path $ CCG.compress $ DBLC.pack $ show contents

readCompressed :: Read a => FilePath -> IO a
readCompressed path = do
  bytes <- DBLC.readFile path
  return $ (read . DBLC.unpack . CCG.decompress) bytes
