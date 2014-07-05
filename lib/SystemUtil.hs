module SystemUtil where

import           Control.Monad
import           System.Exit
import           System.Process
import           System.Random
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
