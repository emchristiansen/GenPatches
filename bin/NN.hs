import           Data.Array.Repa    hiding (extract, map, (++))
import RenderUtil
import MCMC
import SystemUtil
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import Pipes
import qualified Pipes.Prelude as P
import qualified Data.Sequence as Seq
import System.Directory
import Data.Traversable
import SpatialIndex

main :: IO ()
main = do
  files <- getDirectoryContents mvrRoot
  let
    mvrFiles = filter (elem 'h') files
    mvrsIO :: IO [MVR]
    mvrsIO = sequenceA $ map readCompressed mvrFiles
  mvrs <- mvrsIO
  let
    patches = getPatches
  -- f <- mvrFiles
  -- mvr <- readCompressed f :: IO MVR
  return ()
