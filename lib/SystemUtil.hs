module SystemUtil where

import           Codec.Picture
import           Control.Exception
import           Control.Lens          hiding (index)
import           Control.Monad
import           Data.Array.Repa       hiding (extract, map, (++))
import           Data.CSV
import           Data.String.Utils
import           GHC.Float
import           System.Exit
import           System.FilePath.Posix
import           System.Process
import           System.Random
import           Text.Parsec.String
import           Text.Printf

runShell :: String -> IO()
runShell command = do
  putStrLn "Running shell command:"
  putStrLn command
  process <- runCommand command
  exitCode <- waitForProcess process
  case exitCode of
    ExitSuccess -> putStrLn "Shell command finished."
    ExitFailure _ -> error $ printf "Shell command failed: %s" command

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory from to = runShell $ printf "cp -r %s %s" from to

randomString :: Int -> IO String
randomString length' = do
  let chars = ['a' .. 'z'] ++ ['0' .. '9']
  indices' <- replicateM length' $ randomRIO (0, length chars - 1)
  return $ map (chars !!) indices'
