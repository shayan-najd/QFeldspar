module Examples.ImageProcessing.Grayscale where

import Prelude
import QFeldspar.QDSL
import Examples.ImageProcessing.Prelude

grayscale :: Qt (Image -> Image)
grayscale =
  [|| \ image -> $$mkImage
                 ($$heightImage image) ($$widthImage image)
                 (\ i j -> let p = $$getPixel image i j
                               r = $$red   p
                               g = $$green p
                               b = $$blue  p
                               q = ((30 * r) + (59 * g) + (11 * b))
                                   `div` 100
                           in  $$mkPixel q q q)  ||]

run :: IO ()
run = compileImageProcessor "grayscale" grayscale
