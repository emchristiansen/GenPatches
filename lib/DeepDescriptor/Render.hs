module DeepDescriptor.Render where

import qualified Codec.Picture as CP
import qualified Control.Exception as CE
import qualified Control.Lens as CL
import qualified Data.Array.Repa as DAR
import qualified Data.CSV as DC
import qualified Data.String.Utils as DSU
import qualified GHC.Float as GF
import qualified System.FilePath.Posix as SFP
import qualified Text.Parsec.String as TPS
import qualified Text.Printf as TP

import DeepDescriptor.RawStrings
import DeepDescriptor.System

class ShowXML a where
  showXML :: a -> String

data Integrator = RGB | Position | Distance
  deriving (Show, Read)

instance ShowXML Integrator where
  showXML RGB = "<integrator type=\"path\"/>"
  showXML Position = positionString
  showXML Distance = distanceString

CL.declareLenses [d|
  data Sensor = Sensor {
    fovInDegreesL :: Double,
    originL :: DAR.Array DAR.U DAR.DIM1 Double,
    targetL :: DAR.Array DAR.U DAR.DIM1 Double,
    upL :: DAR.Array DAR.U DAR.DIM1 Double,
    widthL :: Int,
    numChannelsL' :: Int,
    sampleCountL :: Int
  } deriving (Show, Read) |]

formatVector :: DAR.Array DAR.U DAR.DIM1 Double -> String
formatVector vector = TP.printf
  "%f,%f,%f"
  (DAR.index vector (DAR.Z DAR.:. 0))
  (DAR.index vector (DAR.Z DAR.:. 1))
  (DAR.index vector (DAR.Z DAR.:. 2))

instance ShowXML Sensor where
  showXML s = TP.printf
    sensorString
    (s CL.^. fovInDegreesL)
    (formatVector $ s CL.^. originL)
    (formatVector $ s CL.^. targetL)
    (formatVector $ s CL.^. upL)
    (s CL.^. sampleCountL)
    (s CL.^. widthL)
    (s CL.^. widthL)
    (case s CL.^. numChannelsL' of
      1 -> "luminance"
      3 -> "rgb"
      _ -> error $ TP.printf
        "numChannels must be 1 or 3, but was %d" $
        s CL.^. numChannelsL')

CL.declareLenses [d|
  data View = View {
    numChannelsL :: Int,
    integratorL :: Integrator,
    sensorL :: Sensor
  } deriving (Show, Read) |]

CL.declareLenses [d|
  data Model = Model {
    directoryL :: FilePath,
    templateL :: String
  } deriving (Show, Read) |]

CL.declareLenses [d|
  -- A Rendering is the direct output of a Mitsuba rendering of a scene.
  -- For this reason, some values in a rendering may be garbage; this can
  -- happen for rays that don't intersect any scene objects.
  data Rendering = Rendering {
    -- This is the HDR color image.
    rgbL :: DAR.Array DAR.U DAR.DIM3 Double,
    -- These are 3D coordinates of each of the pixels in the RGB image.
    positionL :: DAR.Array DAR.U DAR.DIM3 Double,
    -- These is the depth map.
    -- The third dimension is extra, but it is represented as a DIM3
    -- to make the types easier.
    distanceL :: DAR.Array DAR.U DAR.DIM3 Double
  } deriving (Show, Read) |]

makeMitsubaScript :: String -> View -> String
makeMitsubaScript template v =
   let
     replaceIntegrator = DSU.replace "$INTEGRATOR" (showXML $ v CL.^. integratorL)
     replaceSensor = DSU.replace "$SENSOR" (showXML $ v CL.^. sensorL)
    in
      template CL.& replaceIntegrator CL.& replaceSensor

makeSceneDirectory :: FilePath -> String -> IO (String, String)
makeSceneDirectory modelDirectory mitsubaScript = do
  salt <- randomString 8
  let
    directory = SFP.joinPath [
      "/tmp",
      TP.printf "%s_%s" salt $ last $ SFP.splitPath modelDirectory]
    scriptPath = SFP.joinPath [ directory, "script.xml"]
  putStrLn $ TP.printf "Copying %s to %s." modelDirectory directory
  copyDirectory modelDirectory directory
  putStrLn $ TP.printf "Writing Mitsuba script to %s" scriptPath
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
      TP.printf "arr = numpy.load(\"%s\")" npyPath,
      "if len(arr.shape) == 2: arr = numpy.reshape(arr, (arr.shape[0], arr.shape[1], 1))"]
    save i = TP.printf
      "numpy.savetxt(\"%s\", arr[:, :, %d], delimiter=\",\")"
      (TP.printf csvPattern i :: String)
      i
    second = map save [0 .. numChannels - 1]
  in
    unlines $ first ++ second

loadCSVs :: Int -> String -> IO [[[String]]]
loadCSVs numChannels csvPattern = do
  let
    load i = TPS.parseFromFile DC.csvFile $ TP.printf csvPattern i
    right i = do
      Right r' <- load i
      return r'
  mapM right [0 .. numChannels - 1]

parseRenderingComponent :: [[[String]]] -> DAR.Array DAR.U DAR.DIM3 Double
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

  -- print $ show $ length csvs
  -- print $ show $ length $ head csvs
  -- csvs & head & head & length & show & print
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
  putStrLn "\nRendering all 3 components."
  -- print rgb
  r' <- renderComponent m rgb
  -- print position
  p <- renderComponent m position
  -- print distance
  d <- renderComponent m distance
  return $ Rendering r' p d

rgbToImage :: Array U DIM3 Double -> Image PixelRGBF
rgbToImage rgb =
  let
    fromXY x y = PixelRGBF
      (double2Float $ index rgb (Z :. y :. x :. 0))
      (double2Float $ index rgb (Z :. y :. x :. 1))
      (double2Float $ index rgb (Z :. y :. x :. 2))
    Z :. width :. _ :. 3 = extent rgb
  in
    generateImage fromXY width width

distanceToImage :: Array U DIM3 Double -> Image PixelF
distanceToImage distance =
  let
    fromXY x y = (double2Float $ index distance (Z :. y :. x :. 0))
    Z :. width :. _ :. 1 = extent distance
  in
    generateImage fromXY width width

showRendering :: Rendering -> String -> IO()
showRendering r pattern = do
  let
    rgb = printf pattern "rgb"
    -- distance = printf pattern "distance"
  putStrLn $ printf "Writing %s" rgb
  savePngImage rgb $ ImageRGBF $ rgbToImage $ r ^. rgbL
  -- putStrLn $ printf "Writing %s" distance
  -- savePngImage distance $ ImageYF $ distanceToImage $
    -- r ^. distanceL
