module Examples.IPGray.CDSL where

import QFeldspar.CDSL

redCoefficient :: Data Int
redCoefficient   = 30

greenCoefficient :: Data Int
greenCoefficient = 59

blueCoefficient :: Data Int
blueCoefficient  = 11

rgbToGray :: Data Int -> Data Int ->
             Data Int -> Data Int
rgbToGray = \ r -> \ g -> \ b ->
            div
            (add
             (add (mul r redCoefficient)
                      (mul g greenCoefficient))
             (mul b blueCoefficient)) 100

ipgrayVec :: Vec Int -> Vec Int
ipgrayVec = \ v ->
         vec (div (lenV v) 3)
                 (\ i -> let j = shared (mul i 3) in
                         rgbToGray
                         (indV v j)
                         (indV v (add j 1))
                         (indV v (add j 2)))

ipgray :: Data (Ary Int) -> Data (Ary Int)
ipgray a = vec2ary (ipgrayVec (ary2vec a))