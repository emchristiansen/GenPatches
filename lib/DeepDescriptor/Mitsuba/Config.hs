module DeepDescriptor.Mitsuba.Config (
  ShowXML(..),
  ) where

import qualified Data.Array.Repa as DAR
import qualified Control.Lens as CL
import qualified Text.RawString.QQ as TRQ
import qualified Control.Exception as CE
import qualified Formatting as F
import qualified Data.Text.Lazy as DTL
import qualified Data.String.Interpolation as DSI

import DeepDescriptor.MSR

-- | ShowXML is a class of things that can be rendered to XML.
class ShowXML a where
  showXML :: a -> String

instance ShowXML Integrator where
  showXML RGB = "<integrator type=\"path\"/>"
  showXML Position = [TRQ.r|
<integrator type="field">
  <string name="field" value="position"/>
</integrator>
|]
  showXML Depth = [TRQ.r|
<integrator type="field">
  <string name="field" value="distance"/>
</integrator>
|]

-- | formatVector shows a Vector3D in a format suitable for Mitsuba.
formatVector :: Vector3D -> String
formatVector vector = DTL.unpack $ F.format
  (F.float F.% "," F.% F.float F.% "," F.% F.float)
  (DAR.index vector (DAR.Z DAR.:. 0))
  (DAR.index vector (DAR.Z DAR.:. 1))
  (DAR.index vector (DAR.Z DAR.:. 2))

instance ShowXML (Sensor, Int) where
  showXML (s, numChannels) =
    CE.assert ((numChannels == 1) || (numChannels == 3)) $
    let
      d = show $ unDegrees $ s CL.^. cameraFrame CL.^. fovInDegrees
      o = formatVector $ unOrigin $ s CL.^. cameraFrame CL.^. origin
      t = formatVector $ unTarget $ s CL.^. cameraFrame CL.^. target
      u = formatVector $ unUp $ s CL.^. cameraFrame CL.^. up
      sc  = show $ s CL.^. sampleCount
      r = show $ s CL.^. resolution
    in
      [DSI.str|
<sensor type="perspective">
  <float name="nearClip" value="10"/>
  <float name="farClip" value="2800"/>
  <float name="focusDistance" value="1000"/>
  <float name="fov" value="$d$"/>

  <transform name="toWorld">
    <lookAt origin="$o$" target="$t$" up="$u$"/>
  </transform>

  <sampler type="ldsampler">
    <integer name="sampleCount" value="$sc$"/>
  </sampler>

  <film type="mfilm">
    <string name="fileFormat" value="numpy"/>
    <integer name="width" value="$r$"/>
    <integer name="height" value="$r$"/>
    <string name="pixelFormat" value="$show numChannels$"/>
    <rfilter type="gaussian"/>
  </film>
</sensor>|]

-- instance ShowXML (Sensor, Int) where
  -- showXML (s, numChannels) =
    -- CE.assert ((numChannels == 1) || (numChannels == 3)) $
    -- TP.printf
      -- [TRQ.r|
-- <sensor type="perspective">
  -- <float name="nearClip" value="10"/>
  -- <float name="farClip" value="2800"/>
  -- <float name="focusDistance" value="1000"/>
  -- <float name="fov" value="%f"/>

  -- <transform name="toWorld">
    -- <lookAt origin="%s" target="%s" up="%s"/>
  -- </transform>

  -- <sampler type="ldsampler">
    -- <integer name="sampleCount" value="%d"/>
  -- </sampler>

  -- <film type="mfilm">
    -- <string name="fileFormat" value="numpy"/>
    -- <integer name="width" value="%d"/>
    -- <integer name="height" value="%d"/>
    -- <string name="pixelFormat" value="%s"/>
    -- <rfilter type="gaussian"/>
  -- </film>
-- </sensor>|]
      -- (unDegrees $ s CL.^. cameraFrame CL.^. fovInDegrees)
      -- (formatVector $ unOrigin $ s CL.^. cameraFrame CL.^. origin)
      -- (formatVector $ unTarget $ s CL.^. cameraFrame CL.^. target)
      -- (formatVector $ unUp $ s CL.^. cameraFrame CL.^. up)
      -- (s CL.^. sampleCount)
      -- (s CL.^. resolution)
      -- (s CL.^. resolution)
      -- numChannels
