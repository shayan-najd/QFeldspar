module Examples.IPGray.CDSL where
import Prelude hiding (Int,pi,div,foldl,map)
import QFeldspar.CDSL

redCoefficient :: Dp Int
redCoefficient   = 30

greenCoefficient :: Dp Int
greenCoefficient = 59

blueCoefficient :: Dp Int
blueCoefficient  = 11

rgbToGray :: Dp Int -> Dp Int -> Dp Int -> Dp Int
rgbToGray = \ r -> \ g -> \ b ->
            divE
            ((r * redCoefficient)   +
             (g * greenCoefficient) +
             (b * blueCoefficient)) 100

ipgrayVec :: Vec (Dp Int) -> Vec (Dp Int)
ipgrayVec = \ (Vec l f) ->
            Vec (divE l 3)
                 (\ i -> share (i * 3) (\ j ->
                         rgbToGray
                         (f j)
                         (f (j + 1))
                         (f (j + 2))))

ipgray :: Dp (Ary Int) -> Dp (Ary Int)
ipgray = toExpF ipgrayVec
