module RawStrings where

import           Text.RawString.QQ

positionString :: String
positionString = [r|
<integrator type="field">
  <string name="field" value="position"/>
</integrator>
|]

distanceString :: String
distanceString = [r|
<integrator type="field">
  <string name="field" value="distance"/>
</integrator>
|]

sensorString :: String
sensorString = [r|
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
