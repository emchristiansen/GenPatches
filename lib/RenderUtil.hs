module RenderUtil where

import           Codec.Picture
import           Control.Exception
import           Control.Lens          hiding (index)
import           Data.Array.Repa       hiding (extract, map, (++))
import           Data.CSV
import           Data.String.Utils
import           GHC.Float
import           System.Exit
import           System.FilePath.Posix
import           System.Process
import           SystemUtil
import           Text.Parsec.String
import           Text.Printf

class ShowXML a where
  showXML :: a -> String

data Integrator = RGB | Position | Distance
  deriving (Show)

instance ShowXML Integrator where
  showXML RGB = "<integrator type=\"path\"/>"
  showXML Position = unlines [
    "<integrator type=\"field\"/>",
    "<string name=\"field\" value=\"position\"/>",
    "</integrator>"]
  showXML Distance = unlines [
    "<integrator type=\"field\"/>",
    "<string name=\"field\" value=\"distance\"/>",
    "</integrator>"]

declareLenses [d|
  data Sensor = Sensor {
    fovInDegreesL :: Double,
    originL :: Array U DIM1 Double,
    targetL :: Array U DIM1 Double,
    upL :: Array U DIM1 Double,
    widthL :: Int
  } deriving (Show) |]

formatVector :: Array U DIM1 Double -> String
formatVector vector = printf
  "%f,%f,%f"
  (index vector (Z :. 0))
  (index vector (Z :. 1))
  (index vector (Z :. 2))

instance ShowXML Sensor where
  showXML s = unlines [
    "<sensor type=\"perspective\">",
    "<float name=\"nearClip\" value=\"10\"/>",
    "<float name=\"farClip\" value=\"2800\"/>",
    "<float name=\"focusDistance\" value=\"1000\"/>",
    printf "<float name=\"fov\" value=\"%f\"/>" $ s ^. fovInDegreesL,
    "",
    "<transform name=\"toWorld\">",
    printf
      "<lookAt origin=\"%s\" target=\"%s\" up=\"%s\"/>"
      (formatVector $ s ^. originL)
      (formatVector $ s ^. targetL)
      (formatVector $ s ^. upL),
    "</transform>",
    "",
    "<sampler type=\"ldsampler\">",
    "<integer name=\"sampleCount\" value=\"64\"/>",
    "</sampler>",
    "",
    "<film type=\"mfilm\">",
    "<string name=\"fileFormat\" value=\"numpy\"/>",
    printf "<integer name=\"width\" value=\"%d\"/>" $ s ^. widthL,
    printf "<integer name=\"height\" value=\"%d\"/>" $ s ^. widthL,
    "<rfilter type=\"gaussian\"/>",
    "</film>",
    "</sensor>"]

declareLenses [d|
  data View = View {
    numChannelsL :: Int,
    integratorL :: Integrator,
    sensorL :: Sensor
  } deriving (Show) |]

declareLenses [d|
  data Model = Model {
    directoryL :: FilePath,
    templateL :: String
  } deriving (Show) |]

declareLenses [d|
  -- A Rendering is the direct output of a Mitsuba rendering of a scene.
  -- For this reason, some values in a rendering may be garbage; this can
  -- happen for rays that don't intersect any scene objects.
  data Rendering = Rendering {
    -- This is the HDR color image.
    rgb :: Array U DIM3 Double,
    -- These are 3D coordinates of each of the pixels in the RGB image.
    position :: Array U DIM3 Double,
    -- These is the depth map.
    -- The third dimension is extra, but it is represented as a DIM3
    -- to make the types easier.
    distance :: Array U DIM3 Double
  } deriving (Show) |]

makeMitsubaScript :: String -> View -> String
makeMitsubaScript template v =
   let
     replaceIntegrator = replace "$INTEGRATOR" (showXML $ v ^. integratorL)
     replaceSensor = replace "$SENSOR" (showXML $ v ^. sensorL)
    in
      template & replaceIntegrator & replaceSensor

makeSceneDirectory :: FilePath -> String -> IO String
makeSceneDirectory modelDirectory mitsubaScript = do
  salt <- randomString 16
  let
    directory = joinPath [
      "/tmp",
      printf "%s_%s" salt $ last $ splitPath modelDirectory]
    scriptPath = joinPath [ directory, "script.xml"]
  putStrLn $ printf "Copying %s to %s." modelDirectory directory
  copyDirectory modelDirectory directory
  putStrLn $ printf "Writing Mitsuba script to %s" scriptPath
  writeFile
    scriptPath
    mitsubaScript
  return scriptPath

callMitsuba :: FilePath -> FilePath -> IO ()
callMitsuba scriptPath outPath = do
  runShell $ unwords [
    "/usr/bin/mitsuba",
    scriptPath,
    "-o",
    outPath]

makePythonScript :: Int -> String -> String -> String
makePythonScript numChannels npyPath csvPattern =
  let
    first = ["import numpy", printf "arr = numpy.load(\"%s\")" npyPath]
    save i = printf
      "numpy.savetxt(\"%s\", arr[:, :, %d], delimiter=\",\")"
      (printf csvPattern i :: String)
      i
    second = map save [0 .. numChannels - 1]
  in
    unlines $ first ++ second

loadCSVs :: Int -> String -> IO [[[String]]]
loadCSVs numChannels csvPattern = do
  let
    load i = parseFromFile csvFile $ printf csvPattern i
    right i = do
      Right r <- load i
      return r
  mapM right [0 .. numChannels - 1]

parseRenderingComponent :: [[[String]]] -> Array U DIM3 Double
parseRenderingComponent csv =
  let
    numChannels = length csv
    width = length $ head csv
    height = length $ head $ head csv
    doubles :: [[[Double]]]
    doubles = map (map (map read)) csv
    shape = Z :. width :. width :. numChannels
    f (Z :. row :. column :. channel) = doubles !! channel !! row !! column
  in
    assert (width == height)
    computeS $ fromFunction shape f

-- render :: String -> Parameters -> IO Rendering
-- render salt p = do
--   let
--     npyPath = printf "/tmp/render_%s.npy" salt
--     csvPattern = printf "/tmp/render_%s" salt ++ "_%d.csv"
--     pyPath = printf "/tmp/loadcsv_%s.py" salt
--   callMitsuba p npyPath
--   writeFile pyPath $ makePythonScript (p ^. numChannelsL) npyPath csvPattern
--   runShell $ unwords ["/usr/bin/python", pyPath]
--   csvs <- loadCSVs (p ^. numChannelsL) csvPattern
--   -- print csvs
--   print $ show $ length csvs
--   print $ show $ length $ head csvs
--   -- print $ show $ length $ (head . head) csvs
--   csvs & head & head & length & show & print
--   return $ parseRenderingComponent csvs
--
-- showRendering :: Rendering -> String -> IO()
-- showRendering (Rendering rgb) pattern = do
--   let
--     fromXY x y = PixelRGBF
--       (double2Float $ index rgb (Z :. y :. x :. 0))
--       (double2Float $ index rgb (Z :. y :. x :. 1))
--       (double2Float $ index rgb (Z :. y :. x :. 2))
--     Z :. width :. _ :. 3 = extent rgb
--     image = ImageRGBF $ generateImage fromXY width width
--   -- saveRadianceImage (printf pattern "rgb") image
--   savePngImage (printf pattern "rgb") image
