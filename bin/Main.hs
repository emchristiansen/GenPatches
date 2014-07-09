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
import Control.Lens
import MCMC

main :: IO()
main = do
  let
    s = Sensor
      40.0
      (fromListUnboxed (Z :. 3) [278.0, 273.0, -800.0])
      (fromListUnboxed (Z :. 3) [278.0, 273.0, -799.0])
      (fromListUnboxed (Z :. 3) [0.0, 1.0, 0.0])
      32
      3
      128
    integrator = RGB
    v = View 3 integrator s
    m = Model "data/cbox" "cbox.xml"
  -- s' <- perturb s
  -- print s
  -- print s'

  -- r <- render m v
  -- print $ r ^. distanceL
  -- showRendering r "/tmp/rendering_%s.png"

  mcmc m v

  putStrLn "Done"
