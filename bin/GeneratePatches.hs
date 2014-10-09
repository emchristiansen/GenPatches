import qualified Data.Array.Repa as DAR
import qualified Data.Sequence as DS
import qualified Pipes as P
import qualified Pipes.Prelude as PP
import qualified Shelly as S
import qualified Data.Text as DT
import qualified System.FilePath.Posix as SFP
import qualified Control.Monad as CM

import DeepDescriptor.MSR
import DeepDescriptor.MCMC.Iterate
import DeepDescriptor.System

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

outputRoot :: IO String
outputRoot = do
  home <- CM.liftM (fmap DT.unpack) $ S.shelly $ S.get_env "HOME"
  case home of
    Just h -> return $ SFP.joinPath[h, "t/2014_q3/mcmc"]
    Nothing -> undefined

main :: IO ()
main = do
  let
    cf = mkCameraFrame
      (mkDegrees 40.0)
      (Origin $ DAR.fromListUnboxed (DAR.Z DAR.:. 3) [278.0, 273.0, -800.0])
      (Target $ DAR.fromListUnboxed (DAR.Z DAR.:. 3) [278.0, 273.0, -799.0])
      (Up $ DAR.fromListUnboxed (DAR.Z DAR.:. 3) [0.0, 1.0, 0.0])
    s = Sensor
      cf
      256
      128
    -- integrator = RGB
    -- v = View 3 integrator s
    m = Model "data/cbox" "cbox.xml"
  -- mcmc m v []
  outputRoot' <- outputRoot
  S.shelly $ S.mkdir_p $ S.fromText $ DT.pack outputRoot'
  S.shelly $ S.mkdir_p $ S.fromText $ DT.pack $ renderingRoot outputRoot'
  S.shelly $ S.mkdir_p $ S.fromText $ DT.pack $ mvrRoot outputRoot'

  let
    mvrs :: P.Producer MSR IO ()
    mvrs = mcmc m s DS.empty
  P.runEffect $ P.for (mvrs P.>-> PP.take 1) (P.lift . (saveMSR outputRoot'))

  -- s' <- perturb s
  -- print s
  -- print s'

  -- r <- render m v
  -- print $ r ^. distanceL
  -- showRendering r "/tmp/rendering_%s.png"

  putStrLn "Done"
