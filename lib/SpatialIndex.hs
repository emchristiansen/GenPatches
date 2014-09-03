module SpatialIndex where

import Data.Trees.KdTree
import Data.Array.Repa hiding (map)
import qualified Data.Array.Repa as R
import MCMC
import RenderUtil
import qualified Data.Vector as V
import           Control.Lens

type Point3D = Array U DIM1 Double

type Image = Array U DIM3 Double

data IndexedPatch = IndexedPatch Point3D Image

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
    image :: Array U DIM3 Double
    rgbFull :: Array U DIM3 Double
    rgbFull = r ^. rgbL
    image = computeS $ fromFunction
      (Z :. patchWidth :. patchWidth :. 3)
      (\(Z :. r :. c :. z) -> rgbFull R.! (Z :. r + row :. c + column :. z))
  return $ IndexedPatch xyz image
