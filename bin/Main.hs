import           Data.Array.Repa hiding (extract, map)
-- import           Text.RawString.QQ
import           Text.Printf
import           Text.XML.Light

type Model = String
type Origin = Array U DIM1 Double

type Target = Array U DIM1 Double

type Output = String

data RenderParameters = RenderParameters Model Output Origin Target

-- mitsuba building_001_macros.xml -D NUM_SAMPLES=64 -D HEIGHT=512 -D WIDTH=512


callMitsuba :: RenderParameters -> IO ()
callMitsuba (RenderParameters model output origin target) = do
  let
    originString :: String
    originString = printf
      "%f, %f, %f"
      (index origin (Z :. 0))
      (index origin (Z :. 1))
      (index origin (Z :. 2))
    targetString :: String
    targetString = printf
      "%f, %f, %f"
      (index target (Z :. 0))
      (index target (Z :. 1))
      (index target (Z :. 2))
    command :: String
    command = printf
      "mitsuba %s -o %s -D ORIGIN=%s TARGET=%s"
      model
      output
      originString
      targetString
  putStrLn "Sup"


-- makeSensor :: Origin ->

main = do
  -- let xml = [r|<sensor type="perspective"/>|]
  let xml = "<sensor type=\"perspective\"/>"
  putStrLn $ show $ parseXML xml
  putStrLn "Hi"
