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

ipgrayVec :: Vec (Data Int) -> Vec (Data Int)
ipgrayVec = \ v ->
         vec (div (len v) 3)
                 (\ i -> share (mul i 3) (\ j ->
                         rgbToGray
                         (ind v j)
                         (ind v (add j 1))
                         (ind v (add j 2))))

ipgray :: Data (Ary Int) -> Data (Ary Int)
ipgray = toExpF ipgrayVec