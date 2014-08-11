import           Data.Array.Repa    hiding (extract, map, (++))
-- import           Text.RawString.QQ
-- import           System.Process
-- import           Text.Printf
-- import           Text.XML.Light
-- import           Data.CSV
-- import Text.Parsec
-- import           Codec.Picture
-- import           Control.Lens       hiding (index)
-- import           GHC.Float
-- import           Text.Parsec.String
import RenderUtil
-- import Control.Lens
import MCMC
import SystemUtil
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import Pipes
import qualified Pipes.Prelude as P

-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- Person
    -- name String
    -- age Int Maybe
--     deriving Show
-- BlogPost
--     title String
--     authorId PersonId
--     deriving Show
-- |]

-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- MVR
  -- model Model
  -- view View
  -- rendering Rendering
-- |]

main :: IO()
main = do
  let
    s = Sensor
      40.0
      (fromListUnboxed (Z :. 3) [278.0, 273.0, -800.0])
      (fromListUnboxed (Z :. 3) [278.0, 273.0, -799.0])
      (fromListUnboxed (Z :. 3) [0.0, 1.0, 0.0])
      256
      3
      128
    integrator = RGB
    v = View 3 integrator s
    m = Model "data/cbox" "cbox.xml"
  -- mcmc m v []
  makeDirectory outputRoot
  makeDirectory renderingRoot
  makeDirectory mvrRoot

  let mvrs = mcmc2 m v []
  runEffect $ for (mvrs >-> P.take 3) (lift . saveMVR)

  -- s' <- perturb s
  -- print s
  -- print s'

  -- r <- render m v
  -- print $ r ^. distanceL
  -- showRendering r "/tmp/rendering_%s.png"

  putStrLn "Done"
