import           Data.Array.Repa hiding (extract, map)
-- import           Text.RawString.QQ
import           Text.XML.Light

type Origin = Array U DIM1 Double

type LookAt = Array U DIM1 Double



-- makeSensor :: Origin ->

main = do
  -- let xml = [r|<sensor type="perspective"/>|]
  let xml = "<sensor type=\"perspective\"/>"
  putStrLn $ show $ parseXML xml
  putStrLn "Hi"
