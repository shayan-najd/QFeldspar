module Examples.ImageProcessing.GrayscaleFunctional where

import Prelude
import QFeldspar.QDSL
import Examples.ImageProcessing.Prelude

mapImage :: Qt ((Word32 -> Word32 -> Word32 -> Pixel) ->
             Image -> Image)
mapImage = [|| \ f image ->
                $$mkImage ($$heightImage image)
                          ($$widthImage  image)
                          (\ i j -> let p = $$getPixel image i j
                                        r = $$red   p
                                        g = $$green p
                                        b = $$blue  p
                                    in  f r g b)
            ||]

grayscale :: Qt (Image -> Image)
grayscale =
  [|| $$mapImage
      ( \ r g b -> let q = ((30 * r) + (59 * g) + (11 * b))
                               `div` 100
                   in  $$mkPixel q q q) ||]


run :: IO ()
run = compileImageProcessor "grayscale" grayscale
