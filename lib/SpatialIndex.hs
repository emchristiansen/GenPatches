module SpatialIndex where

import Data.Trees.KdTree
import Data.Array.Repa hiding (map)
import qualified Data.Array.Repa as R
import MCMC
import qualified Data.Vector as V

type Point3D = Array U DIM1 Double

type Image = Array U DIM3 Double

data IndexedPatch = IndexedPatch Point3D Image

getPatches :: Int -> MVR -> Vector IndexedPatch
getPatches patchWidth (MVR _ _ r) = V.fromList $ do
  let
    Z :. rows :. columns = extent $ r ^. rgbL
  row <- [0 .. rows - patchWidth]
  column <- [0 .. columns - patchWidth]