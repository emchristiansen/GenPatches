module DeepDescriptor.System where

import qualified Control.Monad as CM
import qualified System.Random as SR
import qualified Data.ByteString.Lazy.Char8 as DBLC
import qualified Codec.Compression.GZip as CCG
import qualified System.FilePath.Posix as SFP
import qualified Shelly as S
import qualified Data.Text as DT
import qualified Formatting as F
import qualified Formatting.ShortFormatters as FS
import qualified Data.Text.Lazy as DTL
import qualified Data.String.Interpolation as DSI

runShell :: String -> IO()
runShell command = do
  -- F.fprint ("Running shell command: " F.% FS.s F.% "\n") command
  putStrLn [DSI.str|Running shell command: $command$|]
  let
    binary : arguments = map DT.pack $ words command
  _ <- S.shelly $ S.silently $ S.run (S.fromText binary) arguments
  return ()

randomString :: Int -> IO String
randomString length' = do
  let chars = ['a' .. 'z'] ++ ['0' .. '9']
  indices' <- CM.replicateM length' $ SR.randomRIO (0, length chars - 1)
  return $ map (chars !!) indices'

renderingRoot :: FilePath -> FilePath
renderingRoot outputRoot = SFP.joinPath [outputRoot, "rendering"]

mvrRoot :: FilePath -> FilePath
mvrRoot outputRoot = SFP.joinPath [outputRoot, "mvr"]

writeCompressed :: Show a => FilePath -> a -> IO ()
writeCompressed path contents =
  DBLC.writeFile path $ CCG.compress $ DBLC.pack $ show contents

readCompressed :: Read a => FilePath -> IO a
readCompressed path = do
  bytes <- DBLC.readFile path
  return $ (read . DBLC.unpack . CCG.decompress) bytes
