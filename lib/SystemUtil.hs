module SystemUtil where

import           Control.Monad
-- import           System.Exit
-- import           System.Process
import           System.Random
import           Text.Printf
import qualified Data.ByteString.Lazy.Char8 as ByteString
-- import qualified Data.ByteString.Char8 as ByteString
import Codec.Compression.GZip
import           System.FilePath.Posix
import qualified Data.Sequence as Seq
-- import Data.Sequence ((<|))
import qualified Data.Foldable as Foldable
import Shelly hiding (FilePath, command)
-- import qualified Shelly as S
import qualified Data.Text as T
default (T.Text)

runShell :: String -> IO()
runShell command = do
  putStrLn "Running shell command:"
  putStrLn command
  let
    binary : arguments = map T.pack $ words command
    -- shelly :: Sh String
    -- shelly = run binary arguments
  _ <- shelly $ silently $ run (fromText  binary) arguments
  return ()
  -- process <- runCommand command
  -- exitCode <- waitForProcess process
  -- case exitCode of
    -- ExitSuccess -> putStrLn "Shell command finished."
    -- ExitFailure _ -> error $ printf "Shell command failed: %s" command

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory from to = runShell $ printf "cp -r %s %s" from to

makeDirectory :: FilePath -> IO ()
makeDirectory dir = runShell $ printf "mkdir -p %s" dir

randomString :: Int -> IO String
randomString length' = do
  let chars = ['a' .. 'z'] ++ ['0' .. '9']
  indices' <- replicateM length' $ randomRIO (0, length chars - 1)
  return $ map (chars !!) indices'

outputRoot :: FilePath
outputRoot = "/home/eric/Downloads/mcmc"

renderingRoot :: FilePath
renderingRoot = joinPath [outputRoot, "rendering"]

mvrRoot :: FilePath
mvrRoot = joinPath [outputRoot, "mvr"]

writeCompressed :: Show a => FilePath -> a -> IO ()
writeCompressed path contents =
  ByteString.writeFile path (compress $ ByteString.pack $ show contents)

readCompressed :: Read a => FilePath -> IO a
readCompressed path = do
  bytes <- ByteString.readFile path
  return $ (read . ByteString.unpack . decompress) bytes
