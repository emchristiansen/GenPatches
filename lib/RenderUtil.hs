module RenderUtil where

import           Codec.Picture
import           Control.Exception
import           Control.Lens          hiding (index)
import           Data.Array.Repa       hiding (extract, map, (++))
import           Data.CSV
import           Data.String.Utils
import           GHC.Float
import           System.FilePath.Posix
import           SystemUtil
import           Text.Parsec.String
import           Text.Printf
import RawStrings

class ShowXML a where
  showXML :: a -> String

data Integrator = RGB | Position | Distance
  deriving (Show)

instance ShowXML Integrator where
  showXML RGB = "<integrator type=\"path\"/>"
  showXML Position = positionString
  showXML Distance = distanceString

declareLenses [d|
  data Sensor = Sensor {
    fovInDegreesL :: Double,
    originL :: Array U DIM1 Double,
    targetL :: Array U DIM1 Double,
    upL :: Array U DIM1 Double,
    widthL :: Int,
    numChannelsL' :: Int,
    sampleCountL :: Int
  } deriving (Show) |]

formatVector :: Array U DIM1 Double -> String
formatVector vector = printf
  "%f,%f,%f"
  (index vector (Z :. 0))
  (index vector (Z :. 1))
  (index vector (Z :. 2))

instance ShowXML Sensor where
  showXML s = printf
    sensorString
    (s ^. fovInDegreesL)
    (formatVector $ s ^. originL)
    (formatVector $ s ^. targetL)
    (formatVector $ s ^. upL)
    (s ^. sampleCountL)
    (s ^. widthL)
    (s ^. widthL)
    (case s ^. numChannelsL' of
      1 -> "luminance"
      3 -> "rgb"
      _ -> error $ printf
        "numChannels must be 1 or 3, but was %d" $
        s ^. numChannelsL')

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
    rgbL :: Array U DIM3 Double,
    -- These are 3D coordinates of each of the pixels in the RGB image.
    positionL :: Array U DIM3 Double,
    -- These is the depth map.
    -- The third dimension is extra, but it is represented as a DIM3
    -- to make the types easier.
    distanceL :: Array U DIM3 Double
  } deriving (Show) |]

makeMitsubaScript :: String -> View -> String
makeMitsubaScript template v =
   let
     replaceIntegrator = replace "$INTEGRATOR" (showXML $ v ^. integratorL)
     replaceSensor = replace "$SENSOR" (showXML $ v ^. sensorL)
    in
      template & replaceIntegrator & replaceSensor

makeSceneDirectory :: FilePath -> String -> IO (String, String)
makeSceneDirectory modelDirectory mitsubaScript = do
  salt <- randomString 8
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
  return (directory, scriptPath)

callMitsuba :: FilePath -> FilePath -> IO ()
callMitsuba scriptPath outPath =
  runShell $ unwords [
    "/usr/bin/mitsuba",
    scriptPath,
    "-o",
    outPath]

makePythonScript :: Int -> String -> String -> String
makePythonScript numChannels npyPath csvPattern =
  let
    first = [
      "import numpy",
      printf "arr = numpy.load(\"%s\")" npyPath,
      "if len(arr.shape) == 2: arr = numpy.reshape(arr, (arr.shape[0], arr.shape[1], 1))"]
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
      Right r' <- load i
      return r'
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

renderComponent :: Model -> View -> IO (Array U DIM3 Double)
renderComponent m v = do
  template <- readFile $ joinPath [m ^. directoryL, m ^. templateL]
  let
    mitsubaScript = makeMitsubaScript template v
  (directory, scriptPath) <- makeSceneDirectory (m ^. directoryL) mitsubaScript
  let
    npyPath = joinPath[directory, "render.npy"]
    csvPattern = joinPath[directory, "render_%d.csv"]
    pythonScript = makePythonScript (v ^. numChannelsL) npyPath csvPattern
    pyPath = joinPath[directory, "npy_to_csvs.py"]
  callMitsuba scriptPath npyPath
  writeFile pyPath pythonScript
  runShell $ unwords ["/usr/bin/python", pyPath]
  csvs <- loadCSVs (v ^. numChannelsL) csvPattern

  print $ show $ length csvs
  print $ show $ length $ head csvs
  csvs & head & head & length & show & print
  return $ parseRenderingComponent csvs

render :: Model -> View -> IO Rendering
render m v = do
  let
    rgb :: View
    rgb = set numChannelsL 3 $ set (sensorL . numChannelsL') 3 $ set integratorL RGB v
    position :: View
    position = set numChannelsL 3 $ set (sensorL . numChannelsL') 3 $ set integratorL Position v
    distance :: View
    distance = set numChannelsL 1 $ set (sensorL . numChannelsL') 1 $ set integratorL Distance v
  print rgb
  r' <- renderComponent m rgb
  print position
  p <- renderComponent m position
  print distance
  d <- renderComponent m distance
  return $ Rendering r' p d

showRendering :: Rendering -> String -> IO()
showRendering r' pattern = do
  let
    rgb = r' ^. rgbL
    fromXY x y = PixelRGBF
      (double2Float $ index rgb (Z :. y :. x :. 0))
      (double2Float $ index rgb (Z :. y :. x :. 1))
      (double2Float $ index rgb (Z :. y :. x :. 2))
    Z :. width :. _ :. 3 = extent rgb
    image = ImageRGBF $ generateImage fromXY width width
  -- saveRadianceImage (printf pattern "rgb") image
  savePngImage (printf pattern "rgb") image
