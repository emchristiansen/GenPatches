module DeepDescriptor.Mitsuba.Config where

import qualified Data.Array.Repa as DAR
import qualified Control.Lens as CL
import qualified Text.RawString.QQ as TRQ
import qualified Text.Printf as TP

class ShowXML a where
  showXML :: a -> String

positionString :: String
positionString = [TRQ.r|
<integrator type="field">
  <string name="field" value="position"/>
</integrator>
|]

distanceString :: String
distanceString = [TRQ.r|
<integrator type="field">
  <string name="field" value="distance"/>
</integrator>
|]

instance ShowXML Integrator where
  showXML RGB = "<integrator type=\"path\"/>"
  showXML Position = positionString
  showXML Distance = distanceString


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
