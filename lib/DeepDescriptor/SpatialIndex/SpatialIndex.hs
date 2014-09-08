module DeepDescriptor.SpatialIndex.SpatialIndex where

import Data.Trees.KdTree
import Data.Array.Repa hiding (map)
import qualified Data.Array.Repa as R
import qualified Data.Vector as V
import           Control.Lens

import DeepDescriptor.MCMC
import DeepDescriptor.Render

-- type Point3D = Array U DIM1 Double

type Image = Array U DIM3 Double

data IndexedPatch = IndexedPatch Point3d Image

instance Point IndexedPatch where
  dimension (IndexedPatch p _) = dimension p
  coord i (IndexedPatch p _) = coord i p
  dist2 (IndexedPatch p0 _) (IndexedPatch p1 _) = dist2 p0 p1

getPatches :: Int -> MVR -> V.Vector IndexedPatch
getPatches patchWidth (MVR _ _ r) = V.fromList $ do
  let
    Z :. rows :. columns :. 3 = extent $ r ^. rgbL
  row <- [0 .. rows - patchWidth]
  column <- [0 .. columns - patchWidth]
  let
    rowCenter = row + patchWidth `div` 2
    columnCenter = column + patchWidth `div` 2
    xyz :: Array U DIM1 Double
    xyz = computeS $ (r ^. positionL) `R.slice` (Any :. rowCenter :. columnCenter :. All)
    point = Point3d
      (xyz `R.linearIndex` 0)
      (xyz `R.linearIndex` 1)
      (xyz `R.linearIndex` 2)
    image :: Array U DIM3 Double
    rgbFull :: Array U DIM3 Double
    rgbFull = r ^. rgbL
    image = computeS $ fromFunction
      (Z :. patchWidth :. patchWidth :. 3)
      (\(Z :. r :. c :. z) -> rgbFull R.! (Z :. r + row :. c + column :. z))
  return $ IndexedPatch point image
