module Graphics.Gloss.DevIL
  ( loadDevILPicture
  , imageToPicture
  , readRepaImage
  ) where

import Data.Array.Repa as R
import Graphics.Gloss
import qualified Data.Array.Repa.Repr.ByteString as RB
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import qualified Data.Array.Repa.IO.DevIL as RD
import qualified Data.ByteString as B
import Data.Word
import Control.Monad
import Data.ByteString.Internal as BI

-- |Load  picture using 'Codec-Image-DevIL' and convert it a bitmap for display by 'Gloss'
loadDevILPicture :: FilePath -> IO (Int, Int, Picture)
loadDevILPicture = liftM (imageToPicture False) . readRepaImage

-- |@repaToPicture cacheMeFlag array@ will convert a 'Repa' RGBA array to a
-- tuple of the number of columns, rows and a bitmap for use with 'Gloss'.
imageToPicture :: Bool -> RD.Image -> (Int, Int, Picture)
imageToPicture b (RD.Grey arr) = img2d b arr
imageToPicture b (RD.RGBA arr) = img3d b arr
imageToPicture b (RD.RGB arr)  = img3d b arr

img2d :: Bool -> Array RF.F DIM2 Word8 -> (Int, Int, Picture)
img2d b arr0 = (oldCol, oldRow, bitmapOfByteString oldRow oldCol (toBS arr0) b)
  where
  e@(Z :. oldCol :. oldRow) = extent arr0
  toBS arr =
    let arr2 = backpermute e order arr
        order (Z :. col :. row) =
                   Z :. col :. row
        fp = RF.toForeignPtr (R.computeS arr2)
    in BI.fromForeignPtr fp 0 (oldCol * oldRow)

img3d :: Bool -> Array RF.F DIM3 Word8 -> (Int, Int, Picture)
img3d b arr0 = (oldCol, oldRow, bitmapOfByteString oldRow oldCol (toBS arr0) b)
  where
  e@(Z :. oldCol :. oldRow :. oldChan) = extent arr0
  toBS arr =
    let arr2 = backpermute e order arr
        order (Z :. col :. row :. chan) =
                   Z :. col :. row :. oldChan - chan - 1
        fp = RF.toForeignPtr (R.computeS arr2)
    in BI.fromForeignPtr fp 0 (oldCol * oldRow * oldChan)

-- |Read in a file into a repa array (using the 'repa-devil' package)
readRepaImage :: FilePath -> IO RD.Image
readRepaImage = RD.runIL . RD.readImage
