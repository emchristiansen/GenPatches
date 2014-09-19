module DeepDescriptor.Mitsuba.Config where

import qualified Data.Array.Repa as DAR
import qualified Control.Lens as CL
import qualified Text.RawString.QQ as TRQ
import qualified Text.Printf as TP
import qualified Control.Exception as CE
import qualified Formatting as F
import qualified Data.Text.Lazy as DTL

import DeepDescriptor.MVR

class ShowXML a where
  showXML :: a -> String

strings :: String
strings = DTL.unpack $ F.format
  ("Here comes a string: " F.% F.string F.% " and another " F.% F.string)
  "Hello, World!"
  "Ahoy!"

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

formatVector :: DAR.Array DAR.U DAR.DIM1 Double -> String
formatVector vector = TP.printf
  "%f,%f,%f"
  (DAR.index vector (DAR.Z DAR.:. 0))
  (DAR.index vector (DAR.Z DAR.:. 1))
  (DAR.index vector (DAR.Z DAR.:. 2))

sensorString :: String
sensorString = [TRQ.r|
<sensor type="perspective">
  <float name="nearClip" value="10"/>
  <float name="farClip" value="2800"/>
  <float name="focusDistance" value="1000"/>
  <float name="fov" value="%f"/>

  <transform name="toWorld">
    <lookAt origin="%s" target="%s" up="%s"/>
  </transform>

  <sampler type="ldsampler">
    <integer name="sampleCount" value="%d"/>
  </sampler>

  <film type="mfilm">
    <string name="fileFormat" value="numpy"/>
    <integer name="width" value="%d"/>
    <integer name="height" value="%d"/>
    <string name="pixelFormat" value="%s"/>
    <rfilter type="gaussian"/>
  </film>
</sensor>|]

instance ShowXML (Sensor, Int) where
  showXML (s, numChannels) =
    CE.assert ((numChannels == 1) || (numChannels == 3)) $
    TP.printf
      sensorString
      (unDegrees $ s CL.^. cameraFrame CL.^. fovInDegrees)
      (formatVector $ s CL.^. cameraFrame CL.^. origin)
      (formatVector $ s CL.^. cameraFrame CL.^. target)
      (formatVector $ s CL.^. cameraFrame CL.^. up)
      (s CL.^. sampleCount)
      (s CL.^. resolution)
      (s CL.^. resolution)
      numChannels
