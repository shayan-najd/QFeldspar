module Examples.Image.SwirlHS where

import Prelude (sin,cos,atan2,round,words)
import QFeldspar.MyPrelude
import QFeldspar.Prelude.Haskell

import System.IO.Unsafe

swirl :: Float -> Vec (Vec (Word32, Word32, Word32))
               -> Vec (Vec (Word32, Word32, Word32))
swirl scale vec =
  let height = len vec
      width  = len (index vec 0)
      h      = i2f height
      w      = i2f width
  in  indexed2 height width  (\i j ->
        let ir = i2f i / h - 0.5
            jr = i2f j / w - 0.5
            r = sqrt(ir * ir + jr * jr)
            theta = atan2 jr ir
            theta1 = 50 * scale * r * (0.5 - r) + theta
            is = round ((0.5 + if r < 0.5
                               then r * cos theta1
                               else ir) * h)
            js = round ((0.5 + if r < 0.5
                               then r * sin theta1
                               else jr) * w)
        in  ix2 vec is js)

readPPM :: String -> Vec (Vec (Word32,Word32,Word32))
readPPM s = unsafePerformIO
         (do f <- readFile s
             let "P3" : size : "255" : c = lines f
                 width : height : _ = fmap read (words size)
                 ls = fmap (read :: String -> Word32) c
             return (Vec height (\i -> Vec width (\j ->
               (ls !! (fromIntegral (j+i*width)*3)
               ,ls !! (fromIntegral (j+i*width)*3 + 1)
               ,ls !! (fromIntegral (j+i*width)*3 + 2))
                      )))
         )

writePPM :: String -> Vec (Vec (Word32,Word32,Word32)) -> IO ()
writePPM s vec = do let height = len vec
                        width  = len (index vec 0)
                        ls = "P3" :
                           (show width ++ " " ++ show height) :
                           "255" :
                           [ show c
                           | i <- [0..height-1]
                           , j <- [0..width-1]
                           , let  (r,g,b) = ix2 vec i j
                           , c <- [r,g,b]
                           ]
                    writeFile s (unlines ls)

indexed2 :: Word32 -> Word32 ->
           (Word32 -> Word32 -> (Word32,Word32,Word32)) ->
            Vec (Vec (Word32,Word32,Word32))
indexed2 height width ixf =
  Vec height (\ i -> Vec width  (\ j -> ixf i j))

len :: Vec a -> Word32
len (Vec l _) = l

index :: Vec a -> Word32 -> a
index (Vec _ ixf) i = ixf i

ix2 :: Vec (Vec a) -> Word32 -> Word32 -> a
ix2 vec i j = index (index vec i) j

test = writePPM "Image2.ppm" (swirl 0.2 (readPPM "Examples/Swirl/Image.ppm"))
