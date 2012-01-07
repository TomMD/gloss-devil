-- Example borrowed from gloss-examples and adapted
import Graphics.Gloss
import Graphics.Gloss.DevIL
import System.Environment
import qualified Data.ByteString as B

main
 = do   args    <- getArgs
        case args of
         [fileName] -> run fileName
         _ -> putStr
           $  unlines [ "usage: bitmap <file.png>"
                      , "  file.png should be a PNG file (32-bit RGBA)"]

run fileName
 = do (width, height, picture) <- loadDevILPicture fileName
      print (width,height) 
      display (FullScreen (1280,1024)) white picture
