{-|
DeepDescriptor.MVR defines common types for DeepDescriptor, including the Model, View, and Rendering types.
-}

module DeepDescriptor.MVR (
  Model(..),
  directory,
  template,
  Integrator(..),
  -- numChannels,
  Vector3D,
  Degrees(),
  mkDegrees,
  unDegrees,
  Origin(..),
  Target(..),
  Up(..),
  CameraFrame(),
  fovInDegrees,
  origin,
  target,
  up,
  mkCameraFrame,
  Sensor(..),
  cameraFrame,
  resolution,
  sampleCount,
  View(..),
  integrator,
  sensor,
  RGBImage,
  PositionMap,
  DepthMap,
  Rendering(..),
  rgb,
  position,
  depth,
  Renderer,
  RGBImageValid,
  PositionMapValid,
  DepthMapValid,
  RenderingValid(..),
  rgbValid,
  positionValid,
  depthValid,
  getMask,
  mkRenderingValid,
  MVR(..),
  ) where

-- import qualified Codec.Picture as CP
import qualified Control.Lens as CL
import qualified Data.Array.Repa as DAR
import qualified Data.Array.Repa.Repr.Vector as DARRV
-- import qualified GHC.Float as GF
-- import qualified Text.Printf as TP
-- import qualified Control.Exception as CE

-- | 'Model' specifies the graphics model (vertices, textures, etc) of the scene.
data Model = Model {
  -- | directory specifies the root of the directory containing the scene data.
  -- The parameterized template is expected to sit in the root of this directory.
  -- TODO: Move this to an in-memory file system.
  _directory :: FilePath,
  -- | template is a file specifying how a scene should be rendered.
  -- It has a number of fields that should be overwritten before the file
  -- is fed as input to Mitsuba.
  -- TODO: Specify these fields.
  _template :: String
} deriving (Show, Read)
CL.makeLenses ''Model

-- | 'Integrator' specifies the output type of the rendering step.
-- It corresponds to the Mitsuba @Integrator@ field.
data Integrator
  -- | 'RGB' renders an 3-channel RGB image.
  -- This can take some time.
  = RGB
  -- | 'Position' returns the 3D locations of all the pixels in the visible
  -- scene.
  | Position
  -- | 'Depth' returns the depth of each pixel in the visible scene.
  | Depth
  deriving (Show, Read)

-- | 'numChannels' specifies the number of image channels for each of the
-- 'Integrator' types.
-- numChannels :: Integrator -> Int
-- numChannels RGB = 3
-- numChannels Position = 3
-- numChannels Depth = 1

-- | 'Vector3D' is a vector in R^3.
type Vector3D = DAR.Array DAR.U DAR.DIM1 Double

-- | Degrees is an angle in degrees which measures field of view.
-- Invariant: It is in the range (0.0, 180.0).
data Degrees = Degrees { unDegrees :: Double } deriving (Show, Read)
mkDegrees :: Double -> Maybe Degrees
mkDegrees d =
  if d > 0.0 && d < 180.0
     then Just $ Degrees d
     else Nothing

data Origin = Origin { unOrigin :: Vector3D } deriving (Show, Read)
data Target = Target { unTarget :: Vector3D } deriving (Show, Read)
data Up = Up { unUp :: Vector3D } deriving (Show, Read)

-- | CameraFrame specifies the 3D location, direction, and field of view
-- of the virtual camera.
-- Invariant: 'up' is orthogonal to 'target' - 'origin'.
data CameraFrame = CameraFrame {
  _fovInDegrees :: Degrees,
  _origin :: Origin,
  _target :: Target,
  _up :: Up
} deriving (Show, Read)
CL.makeLenses ''CameraFrame

-- | mkCameraFrame constructs a 'CameraFrame'.
mkCameraFrame
  :: Degrees -- ^ 'fovInDegrees' is the field of view in degrees.
  -> Origin -- ^ 'origin' is the camera center.
  -> Target -- ^ 'target' is the point at which the camera is looking.
  -> Up -- ^ 'up' is the up vector of the camera.
             -- It must be orthogonal to 'target' - 'origin'.
  -> Maybe CameraFrame
mkCameraFrame f o t u =
  let
    lookDirection = (unTarget t) DAR.-^ (unOrigin o)
    dotProduct = DAR.sumAllS $ lookDirection DAR.*^ (unUp u)
  in
    if ((abs dotProduct) < 0.00001)
       then Just $ CameraFrame f o t u
       else Nothing

-- | 'Sensor' specifies the camera location in the scene and some of its
-- rendering properties.
-- It corresponds to the Mitsuba @Sensor@ field.
data Sensor = Sensor {
  -- | 'cameraFrame' gives the camera parameters for this rendering.
  _cameraFrame :: CameraFrame,
  -- | 'resolution' is the width in pixels of the rendered image, which
  -- is square.
  _resolution :: Int,
  -- | 'sampleCount' is the number of ray-tracing samples to take at each
  -- pixel.
  -- The larger the count, the better the quality; a decent quality might
  -- be achieved at around 32.
  _sampleCount :: Int
} deriving (Show, Read)
CL.makeLenses ''Sensor

-- | 'View' specifies the camera location in the scene as well as its complete
-- rendering properties.
data View =
  View {
    -- | 'integrator' specifies the kind of image that will be created.
    _integrator :: Integrator,
    -- | 'sensor' specifies camera location and some rendering properties.
    _sensor :: Sensor
  } deriving (Show, Read)
CL.makeLenses ''View

-- | RGBImage is a 3 channel color image.
-- It may contain junk values.
type RGBImage = DAR.Array DAR.U DAR.DIM3 Double

-- | PositionMap is a 3 channel array where each element is the 3D location in
-- the scene of the point under that pixel.
-- It may contain junk values.
type PositionMap = DAR.Array DAR.U DAR.DIM3 Double

-- | DepthMap is a 1 channel array given the distance from the camera of each point.
-- It may contain junk values.
type DepthMap = DAR.Array DAR.U DAR.DIM2 Double

-- | 'Rendering' is the direct output of a Mitsuba rendering of a scene.
-- For this reason, some values in a rendering may be garbage; this can
-- happen for rays that don't intersect any scene objects.
-- TODO: Invariant: All elements have the same width and height.
data Rendering = Rendering {
  -- 'rgb' is the HDR color image.
  _rgb :: RGBImage,
  -- 'position' is the 3D coordinates of each of the pixels in the RGB image.
  _position :: PositionMap,
  -- 'depth' is the is the distance from the camera center to the object
  -- under each pixel.
  _depth :: DepthMap
} deriving (Show, Read)
CL.makeLenses ''Rendering

-- | 'Renderer' is a type that takes a 'Model' and renders it from
-- the given 'View'.
type Renderer = Model -> (Integrator -> View) -> IO Rendering

-- | 'RGBImageValid' is like 'RGBImage' but only contains valid values.
type RGBImageValid = DAR.Array DARRV.V DAR.DIM3 (Maybe Double)

-- | 'PositionMapValid' is like 'PositionMap' but only contains valid values.
type PositionMapValid = DAR.Array DARRV.V DAR.DIM3 (Maybe Double)

-- | 'DepthMapValid' is like 'DepthMap' but only contains valid values.
type DepthMapValid = DAR.Array DARRV.V DAR.DIM2 (Maybe Double)

-- | 'RenderingValid' is like 'Rendering' but only contains valid values.
-- TODO: Invariant: All elements have the same width and height.
data RenderingValid = RenderingValid {
  -- 'rgbValid' is valid RGB values.
  _rgbValid :: RGBImageValid,
  -- 'positionValid' is valid 3D positions.
  _positionValid :: PositionMapValid,
  -- 'depthValid' is valid depths.
  _depthValid :: DepthMapValid
} deriving (Show, Read)
CL.makeLenses ''RenderingValid

-- | getMask detects which pixels in a rendering are valid by inspecting
-- the depth map values.
-- It is a helper for mkRenderingValid.
getMask :: DepthMap -> DAR.Array DAR.U DAR.DIM2 Bool
getMask d = DAR.computeS $ DAR.map (\x -> x >= 0.01 && x <= 2800.0) d

-- | mkrenderingValid creates a valid rendering from a raw rendering by
-- detecting and masking out invalid pixels.
mkRenderingValid :: Rendering -> RenderingValid
mkRenderingValid r =
  let
    mask = getMask $ r CL.^. depth
    from3D :: DAR.Array DAR.U DAR.DIM3 Double -> RGBImageValid
    from3D array = DAR.computeS $ DAR.fromFunction
      (DAR.extent array)
      (\location @ (DAR.Z DAR.:. row DAR.:. column DAR.:. 3) ->
        if DAR.index mask (DAR.Z DAR.:. row DAR.:. column)
           then Just $ DAR.index array location
           else Nothing)
    rgb' :: RGBImageValid
    rgb' = from3D $ r CL.^. rgb
    position' :: PositionMapValid
    position' = from3D $ r CL.^. position
    depth' :: DepthMapValid
    depth' = DAR.computeS $ DAR.fromFunction
      (DAR.extent $ r CL.^. depth)
      (\location ->
        if DAR.index mask location
           then Just $ DAR.index (r CL.^. depth) location
           else Nothing)
  in
    RenderingValid rgb' position' depth'

data MVR = MVR Model View RenderingValid deriving (Show, Read)

-- rgbToImage :: DAR.Array DAR.U DAR.DIM3 Double -> CP.Image CP.PixelRGBF
-- rgbToImage rgb =
--   let
--     fromXY x y = CP.PixelRGBF
--       (GF.double2Float $ DAR.index rgb (DAR.Z DAR.:. y DAR.:. x DAR.:. 0))
--       (GF.double2Float $ DAR.index rgb (DAR.Z DAR.:. y DAR.:. x DAR.:. 1))
--       (GF.double2Float $ DAR.index rgb (DAR.Z DAR.:. y DAR.:. x DAR.:. 2))
--     DAR.Z DAR.:. width DAR.:. _ DAR.:. 3 = DAR.extent rgb
--   in
--     CP.generateImage fromXY width width

-- distanceToImage :: DAR.Array DAR.U DAR.DIM3 Double -> CP.Image CP.PixelF
-- distanceToImage distance =
--   let
--     fromXY x y = (GF.double2Float $ DAR.index distance (DAR.Z DAR.:. y DAR.:. x DAR.:. 0))
--     DAR.Z DAR.:. width DAR.:. _ DAR.:. 1 = DAR.extent distance
--   in
--     CP.generateImage fromXY width width

-- showRendering :: Rendering -> String -> IO()
-- showRendering r pattern = do
--   let
--     rgb = TP.printf pattern "rgb"
--     -- distance = printf pattern "distance"
--   putStrLn $ TP.printf "Writing %s" rgb
--   CP.savePngImage rgb $ CP.ImageRGBF $ rgbToImage $ r CL.^. rgbL
--   -- putStrLn $ printf "Writing %s" distance
--   -- savePngImage distance $ ImageYF $ distanceToImage $
--     -- r ^. distanceL
