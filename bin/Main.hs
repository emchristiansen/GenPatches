import           Data.Array.Repa hiding (extract, map, (++))
-- import           Text.RawString.QQ
import           System.Process
import           Text.Printf
-- import           Text.XML.Light
import  Data.CSV
-- import Text.Parsec
import Text.Parsec.String
import Codec.Picture
import GHC.Float

type Model = String
type Origin = Array U DIM1 Double
type Target = Array U DIM1 Double
type Up = Array U DIM1 Double
-- type Width = Int

type Output = String

data RenderParameters = RenderParameters Model Origin Target Up

-- mitsuba building_001_macros.xml -D NUM_SAMPLES=64 -D HEIGHT=512 -D WIDTH=512

numChannels :: Int
numChannels = 3
width :: Int
width = 128

runShell :: String -> IO()
runShell command = do
  putStrLn "About to run:"
  putStrLn command
  process <- runCommand command
  _ <- waitForProcess process
  putStrLn "Done"

callMitsuba :: RenderParameters -> String -> IO ()
callMitsuba (RenderParameters model origin target up) output = do
  let
    originString :: String
    originString = printf
      "%f,%f,%f"
      (index origin (Z :. 0))
      (index origin (Z :. 1))
      (index origin (Z :. 2))
    targetString :: String
    targetString = printf
      "%f,%f,%f"
      (index target (Z :. 0))
      (index target (Z :. 1))
      (index target (Z :. 2))
    upString :: String
    upString = printf
      "%f,%f,%f"
      (index up (Z :. 0))
      (index up (Z :. 1))
      (index up (Z :. 2))
    args =
      [ model
      , "-o"
      , output
      , "-D"
      , printf "ORIGIN=\"%s\"" originString
      , "-D"
      , printf "TARGET=\"%s\"" targetString
      , "-D"
      , printf "UP=\"%s\"" upString
      , "-D"
      , printf "WIDTH=\"%s\"" (show width)]
    -- command :: String
    -- command = printf
      -- "mitsuba %s -o %s -D ORIGIN=\"%s\" TARGET=\"%s\" UP=\"%s\""
      -- model
      -- output
      -- originString
      -- targetString
      -- upString
  -- putStrLn command
  putStrLn $ unwords args
  runShell $ unwords ("/usr/bin/mitsuba" : args)
  -- _ <- createProcess (proc "/usr/bin/mitsuba" args)
  -- _ <- createProcess (shell
  -- putStrLn $ show x
  putStrLn "Sup"

pythonScript :: String -> String -> String
pythonScript npyPath csvPattern =
  let
    first = ["import numpy", printf "arr = numpy.load(\"%s\")" npyPath]
    save i = printf
      "numpy.savetxt(\"%s\", arr[:, :, %d], delimiter=\",\")"
      (printf csvPattern i :: String)
      i
    second = map save [0 .. numChannels - 1]
  in
    unlines $ first ++ second

loadCSVs :: String -> IO [[[String]]]
loadCSVs csvPattern = do
  let
    load i = parseFromFile csvFile $ printf csvPattern i
    right i = do
      Right r <- load i
      return r
  mapM right [0 .. numChannels - 1]

data Rendering = Rendering (Array U DIM3 Double) deriving (Show)

getRendering :: [[[String]]] -> Rendering
getRendering csv =
  let
    doubles :: [[[Double]]]
    doubles = map (map (map read)) csv
    -- all :: Array U DIM3 Double
    -- all = fromListUnboxed (Z :. numRows :. numColumns :. numChannels) doubles
    shape = Z :. width :. width :. numChannels
    f (Z :. row :. column :. channel) = doubles !! channel !! row !! column
  in
    Rendering
      (computeS $ fromFunction shape f)
      -- (fromListUnboxed ) (take 3 doubles))
      -- (slice all (Z :. All :. All :. (Range 0 numChannels)))

(|>) :: forall t t1. t1 -> (t1 -> t) -> t
(|>) x y = y x

render :: String -> RenderParameters -> IO Rendering
render salt p = do
  let
    npyPath = printf "/tmp/render_%s.npy" salt
    csvPattern = printf "/tmp/render_%s" salt ++ "_%d.csv"
    pyPath = printf "/tmp/loadcsv_%s.py" salt
  callMitsuba p npyPath
  writeFile pyPath $ pythonScript npyPath csvPattern
  runShell $ unwords ["/usr/bin/python", pyPath]
  csvs <- loadCSVs csvPattern
  -- print csvs
  print $ show $ length csvs
  print $ show $ length $ head csvs
  -- print $ show $ length $ (head . head) csvs
  print $ csvs |> head |> head |> length |> show
  return $ getRendering csvs

showRendering :: Rendering -> String -> IO()
showRendering (Rendering rgb) pattern = do
  let
    fromXY x y = PixelRGBF
      (double2Float $ index rgb (Z :. y :. x :. 0))
      (double2Float $ index rgb (Z :. y :. x :. 1))
      (double2Float $ index rgb (Z :. y :. x :. 2))
    image = ImageRGBF $ generateImage fromXY width width
  -- saveRadianceImage (printf pattern "rgb") image
  savePngImage (printf pattern "rgb") image

main :: IO()
main = do
  let
    model = "data/cbox/cbox.xml"
    -- output = "/tmp/render.npy"
    origin = fromListUnboxed (Z :. 3) [278.0, 273.0, -800.0]
    target = fromListUnboxed (Z :. 3) [278.0, 273.0, -799.0]
    up = fromListUnboxed (Z :. 3) [0.0, 1.0, 0.0]
    renderParameters = RenderParameters model origin target up
  rendering <- render "test0" renderParameters
  -- showRendering rendering "/tmp/rendering_%s.hdr"
  showRendering rendering "/tmp/rendering_%s.png"
  -- print rendering
  -- callMitsuba renderParameters output
  putStrLn "Done"
