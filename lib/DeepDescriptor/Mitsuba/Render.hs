module DeepDescriptor.Mitsuba.Render (
  render,
  ) where

-- import qualified Codec.Picture as CP
import qualified Control.Exception as CE
import qualified Control.Lens as CL
import qualified Data.Array.Repa as DAR
import qualified Data.CSV as DC
import qualified Data.String.Utils as DSU
-- import qualified GHC.Float as GF
import qualified System.FilePath.Posix as SFP
import qualified Text.Parsec.String as TPS
-- import qualified Text.Printf as TP
import qualified Control.Monad as CM
import qualified Shelly as S
import qualified Data.Text as DT
import qualified Formatting as F
import qualified Formatting.ShortFormatters as FS
import qualified Data.Text.Lazy as DTL
import qualified Data.String.Interpolation as DSI


import DeepDescriptor.System
import DeepDescriptor.MSR
import DeepDescriptor.Mitsuba.Config

makeMitsubaScript :: String -> Int -> View -> String
makeMitsubaScript template' numChannels v =
   let
     replaceIntegrator = DSU.replace "$INTEGRATOR" (showXML $ v CL.^. integrator)
     replaceSensor = DSU.replace "$SENSOR" (showXML $ (v CL.^. sensor, numChannels))
    in
      template' CL.& replaceIntegrator CL.& replaceSensor

makeSceneDirectory :: FilePath -> String -> IO (String, String)
makeSceneDirectory modelDirectory mitsubaScript = do
  salt <- randomString 8
  let
    last' = last $ SFP.splitPath modelDirectory
    directory' = SFP.joinPath [
      "/tmp",
      [DSI.str|$salt$_$last'$|]]
    scriptPath = SFP.joinPath [ directory', "script.xml"]
  putStrLn $ [DSI.str|Copying $modelDirectory$ to $directory'$.|]
  S.shelly $ S.cp_r
    (S.fromText $ DT.pack modelDirectory)
    (S.fromText $ DT.pack directory')
  putStrLn $ [DSI.str|Writing Mitsuba script to $scriptPath$|]
  writeFile
    scriptPath
    mitsubaScript
  return (directory', scriptPath)

callMitsuba :: FilePath -> FilePath -> IO ()
callMitsuba scriptPath outPath =
  runShell $ unwords [
    "/usr/bin/mitsuba",
    scriptPath,
    "-o",
    outPath]

makePythonScript :: Int -> String -> (Int -> String) -> String
makePythonScript numChannels npyPath csvPattern =
  let
    first = [
      "import numpy",
      [DSI.str|arr = numpy.load("$npyPath$")|],
      "if len(arr.shape) == 2: arr = numpy.reshape(arr, (arr.shape[0], arr.shape[1], 1))"]
    save i =
      let
        cpi = csvPattern i :: String
      in
       [DSI.str|numpy.savetxt("$cpi$", arr[:, :, $show i$], delimiter=",")|]
    second = map save [0 .. numChannels - 1]
  in
    unlines $ first ++ second

loadCSVs :: Int -> (Int -> String) -> IO [[[String]]]
loadCSVs numChannels csvPattern = do
  let
    load i = TPS.parseFromFile DC.csvFile $ csvPattern i
    right i = do
      Right r' <- load i
      return r'
  mapM right [0 .. numChannels - 1]

-- TODO: Encode numChannels at the type level.
parseRenderingComponent :: [[[String]]] -> DAR.Array DAR.U DAR.DIM3 Double
parseRenderingComponent csv =
  let
    numChannels = length csv
    width = length $ head csv
    height = length $ head $ head csv
    doubles :: [[[Double]]]
    doubles = map (map (map read)) csv
    shape = DAR.Z DAR.:. height DAR.:. width DAR.:. numChannels
    f (DAR.Z DAR.:. row DAR.:. column DAR.:. channel) = doubles !! channel !! row !! column
  in
    CE.assert (width == height)
    DAR.computeS $ DAR.fromFunction shape f

renderComponent :: Model -> View -> IO (DAR.Array DAR.U DAR.DIM3 Double)
renderComponent m v = do
  template' <- readFile $ SFP.joinPath [m CL.^. directory, m CL.^. template]
  let
    numChannels :: Int
    numChannels = case v CL.^. integrator of
      RGB -> 3
      Position -> 3
      Depth -> 1
    mitsubaScript = makeMitsubaScript template' numChannels v
  (directory', scriptPath) <- makeSceneDirectory (m CL.^. directory) mitsubaScript
  let
    npyPath = SFP.joinPath[directory', "render.npy"]
    csvPattern :: Int -> String
    csvPattern i = SFP.joinPath[
      directory',
      [DSI.str|render_$show i$.csv|]]
    pythonScript = makePythonScript numChannels npyPath csvPattern
    pyPath = SFP.joinPath[directory', "npy_to_csvs.py"]
  callMitsuba scriptPath npyPath
  writeFile pyPath pythonScript
  runShell $ unwords ["/usr/bin/python", pyPath]
  csvs <- loadCSVs numChannels csvPattern

  return $ parseRenderingComponent csvs

-- | render is a Renderer.
-- It shells out to Mitsuba to do the actual work.
-- TODO: Make robust to transient failures.
render :: Renderer
render m s = do
  let
    dropThirdDimension :: (DAR.Array DAR.U DAR.DIM3 Double) -> (DAR.Array DAR.U DAR.DIM2 Double)
    dropThirdDimension array3 =
      let
        DAR.Z DAR.:. height DAR.:. width DAR.:. 3 = DAR.extent array3
      in
        DAR.computeS $ DAR.fromFunction
          (DAR.Z DAR.:. height DAR.:. width)
          (\(DAR.Z DAR.:. h DAR.:. w) -> array3 `DAR.index` (DAR.Z DAR.:. h DAR.:. w DAR.:. 1))
  putStrLn "\nRendering all 3 components."
  r <- renderComponent m $ View RGB s
  p <- renderComponent m $ View Position s
  d <- CM.liftM dropThirdDimension $ renderComponent m $ View Depth s
  return $ Rendering r p d
