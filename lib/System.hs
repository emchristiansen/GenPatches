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

runShell :: String -> IO()
runShell command = do
  putStrLn "Running shell command:"
  putStrLn command
  process <- runCommand command
  exitCode <- waitForProcess process
  case exitCode of
    ExitSuccess -> putStrLn "Shell command finished."
    ExitFailure _ -> error $ printf "Shell command failed: %s" command
