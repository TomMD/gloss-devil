module Graphics.Gloss.DevIL
  ( loadDevILPicture
  , repaToPicture
  , readRepaImage
  ) where

import Data.Array.Repa as R
import Graphics.Gloss
import qualified Data.Array.Repa.ByteString as RB
import qualified Data.Array.Repa.IO.DevIL as RD
import qualified Data.ByteString as B
import Data.Word
import Control.Monad

-- |Load  picture using 'Codec-Image-DevIL' and convert it a bitmap for display by 'Gloss'
loadDevILPicture :: FilePath -> IO (Int, Int, Picture)
loadDevILPicture = liftM (repaToPicture False) . readRepaImage

-- |@repaToPicture cacheMeFlag array@ will convert a 'Repa' RGBA array to a tuple of
-- the number of columns, rows and a bitmap for use with 'Gloss'.
repaToPicture :: Bool -> Array DIM3 Word8 -> (Int, Int, Picture)
repaToPicture b arr = (col, row, bitmapOfByteString row col (toBS arr) b)
  where
  e@(Z :. col :. row :. chan) = extent arr
  order (Z :. oldCol :. oldRow :. oldChan) = Z :. oldCol :. oldRow :. chan - oldChan - 1
  toBS = RB.toByteString . backpermute e order

-- |Read in a file into a repa array (using the 'repa-devil' package)
readRepaImage :: FilePath -> IO (Array DIM3 Word8)
readRepaImage = RD.runIL . RD.readImage
