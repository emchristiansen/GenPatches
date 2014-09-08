{-|
DeepDescriptor.MVR defines common types for DeepDescriptor, including the Model, View, and Rendering types.
-}

module DeepDescriptor.MVR (
  Model(..),
  Integrator(..),
  numChannels,
  Vector3D,
  Degrees(),
  mkDegrees,
  CameraFrame(),
  fovInDegrees,
  origin,
  target,
  mkCameraFrame,
  Sensor(),
  ) where

import qualified Data.Array.Repa as DAR
import qualified Control.Lens as CL
import qualified Codec.Picture as CP
import qualified Text.Printf as TP
import qualified GHC.Float as GF

-- | 'Model' specifies the graphics model (vertices, textures, etc) of the scene.
data Model = Model {
  -- TODO: Remove reference to filesystem.
  _directory :: FilePath,
  -- | templateL is test.
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
numChannels :: Integrator -> Int
numChannels RGB = 3
numChannels Position = 3
numChannels Depth = 1

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

-- | CameraFrame specifies the 3D location, direction, and field of view
-- of the virtual camera.
data CameraFrame = CameraFrame {
  _fovInDegrees :: Degrees,
  _origin :: Vector3D,
  _target :: Vector3D,
  _up :: Vector3D
} deriving (Show, Read)
CL.makeLenses ''CameraFrame

-- | mkCameraFrame constructs a 'CameraFrame'.
mkCameraFrame
  :: Degrees -- ^ 'fovInDegrees' is the field of view in degrees.
  -> Vector3D -- ^ 'origin' is the camera center.
  -> Vector3D -- ^ 'target' is the point at which the camera is looking.
  -> Vector3D -- ^ 'up' is the up vector of the camera.
             -- Invariant: It is orthogonal to 'target' - 'origin'.
  -> Maybe CameraFrame
mkCameraFrame f o t u = undefined

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

type RGBImage = DAR.Array DAR.U DAR.DIM3 Double
type PositionMap = DAR.Array DAR.U DAR.DIM3 Double
type DepthMap = DAR.Array DAR.U DAR.DIM1 Double

-- : 'Rendering' is the direct output of a Mitsuba rendering of a scene.
-- For this reason, some values in a rendering may be garbage; this can
-- happen for rays that don't intersect any scene objects.
data Rendering = Rendering {
  -- This is the HDR color image.
  _rgbL :: RGBImage,
  -- These are 3D coordinates of each of the pixels in the RGB image.
  _position :: PositionMap,
  -- These is the depth map.
  -- The third dimension is extra, but it is represented as a DIM3
  -- to make the types easier.
  _depth :: DepthMap
} deriving (Show, Read)
CL.makeLenses ''Rendering

type Renderer = Model -> View -> IO Rendering

data MVR = MVR Model View Rendering deriving (Show, Read)

rgbToImage :: DAR.Array DAR.U DAR.DIM3 Double -> CP.Image CP.PixelRGBF
rgbToImage rgb =
  let
    fromXY x y = CP.PixelRGBF
      (GF.double2Float $ DAR.index rgb (DAR.Z DAR.:. y DAR.:. x DAR.:. 0))
      (GF.double2Float $ DAR.index rgb (DAR.Z DAR.:. y DAR.:. x DAR.:. 1))
      (GF.double2Float $ DAR.index rgb (DAR.Z DAR.:. y DAR.:. x DAR.:. 2))
    DAR.Z DAR.:. width DAR.:. _ DAR.:. 3 = DAR.extent rgb
  in
    CP.generateImage fromXY width width

distanceToImage :: DAR.Array DAR.U DAR.DIM3 Double -> CP.Image CP.PixelF
distanceToImage distance =
  let
    fromXY x y = (GF.double2Float $ DAR.index distance (DAR.Z DAR.:. y DAR.:. x DAR.:. 0))
    DAR.Z DAR.:. width DAR.:. _ DAR.:. 1 = DAR.extent distance
  in
    CP.generateImage fromXY width width

showRendering :: Rendering -> String -> IO()
showRendering r pattern = do
  let
    rgb = TP.printf pattern "rgb"
    -- distance = printf pattern "distance"
  putStrLn $ TP.printf "Writing %s" rgb
  CP.savePngImage rgb $ CP.ImageRGBF $ rgbToImage $ r CL.^. rgbL
  -- putStrLn $ printf "Writing %s" distance
  -- savePngImage distance $ ImageYF $ distanceToImage $
    -- r ^. distanceL
