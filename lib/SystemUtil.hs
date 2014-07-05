module SystemUtil where

import           Data.Array.Repa hiding (extract, map, (++))
import           System.Process
import           Text.Printf
import  Data.CSV
import Text.Parsec.String
import Codec.Picture
import GHC.Float
import Control.Lens hiding (index)
import Control.Exception
import System.Exit
import Data.String.Utils
import System.FilePath.Posix
import System.Random

runShell :: String -> IO()
runShell command = do
  putStrLn "Running shell command:"
  putStrLn command
  process <- runCommand command
  exitCode <- waitForProcess process
  case exitCode of
    ExitSuccess -> putStrLn "Shell command finished."
    ExitFailure _ -> error $ printf "Shell command failed: %s" command

keysChar = ['a'..'z']
keysNum = ['0'..'9']

randomString :: Int -> IO String
randomString length' = do
  let
    chars = ['a' .. 'z'] ++ ['0' .. '9']
  indices <- sequence $ replicateIO $ randomIO (0, length chars - 1)
  return $ map (chars !!) indices
