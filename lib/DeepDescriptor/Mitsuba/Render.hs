module DeepDescriptor.Mitsuba.Render where

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

render :: Renderer
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

