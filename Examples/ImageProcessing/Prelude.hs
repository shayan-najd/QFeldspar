module Examples.ImageProcessing.Prelude where

import Prelude
import QFeldspar.QDSL

lenV :: Qt (Vec a -> Word32)
lenV = [|| \ (Vec l _ixf) -> l ||]

indV :: Qt (Vec a -> Word32 -> a)
indV = [|| \ (Vec _l ixf) -> ixf ||]

type Pixel = (Word32,(Word32,Word32))

mkPixel :: Qt (Word32 -> Word32 -> Word32 -> Pixel)
mkPixel = [|| \ r g b -> (r , (g , b)) ||]

red :: Qt (Pixel -> Word32)
red = [|| \ pixel -> fst pixel ||]

green :: Qt (Pixel -> Word32)
green = [|| \ pixel -> fst (snd pixel) ||]

blue :: Qt (Pixel -> Word32)
blue = [|| \ pixel -> snd (snd pixel) ||]

-- non-empty image
type Image = Vec (Vec Pixel)

mkImage :: Qt (Word32 -> Word32 ->
                (Word32 -> Word32 -> Pixel) -> Image)
mkImage = [|| \ height width ixf ->
  Vec height (\ i ->
    Vec width (\ j -> ixf i j)) ||]

heightImage :: Qt (Image -> Word32)
heightImage = [|| \ image -> $$lenV image ||]

widthImage :: Qt (Image -> Word32)
widthImage = [|| \ image -> $$lenV ($$indV image 0) ||]

getPixel :: Qt (Image -> Word32 -> Word32 -> Pixel)
getPixel = [|| \ vec i j -> $$indV ($$indV vec i) j ||]

aryToImage :: Qt (Word32 -> Word32 -> Ary Word32 -> Image)
aryToImage = [|| \ height width as ->
                 $$mkImage height width (\ i j ->
                     $$mkPixel (ixArr as ((j+i*width)*3))
                               (ixArr as ((j+i*width)*3+1))
                               (ixArr as ((j+i*width)*3+2))) ||]

imageToAry :: Qt (Image -> Ary Word32)
imageToAry = [|| \ image -> let height = $$heightImage image
                                width  = $$widthImage  image
                            in  mkArr (height * width * 3)
                                (\ ii -> let i = div (div ii 3) width
                                             j = mod (div ii 3) width
                                             p = $$getPixel image i j
                                         in if mod ii 3 == 0
                                            then $$red   p
                                            else if mod ii 3 == 1
                                            then $$green p
                                            else $$blue  p) ||]

compileImageProcessor :: String -> Qt (Image -> Image) -> IO ()
compileImageProcessor name f =
  makeIPAt "./Examples/ImageProcessing/GeneratedCode/"
    (compileFunction
       [|| \ imageArray ->
           let l = round (sqrt (i2f (div (lnArr imageArray) 3)))
           in  $$imageToAry ($$f ($$aryToImage l l imageArray))||])
       name

{-
fmapImage :: Qt((Pixel -> Pixel) -> Image -> Image)
fmapImage = [|| \ f image ->
                $$mkImage ($$heightImage image)
                          ($$widthImage  image)
                          (\ i j -> f ($$getPixel image i j))
            ||]
-}
