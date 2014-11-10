module Examples.IPGray.CDSL where
import Prelude hiding (Int,pi,div,foldl,map)
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
ipgrayVec = \ (Vec l f) ->
            Vec (div l 3)
                 (\ i -> share (mul i 3) (\ j ->
                         rgbToGray
                         (f j)
                         (f (add j 1))
                         (f (add j 2))))

ipgray :: Data (Ary Int) -> Data (Ary Int)
ipgray = toExpF ipgrayVec
