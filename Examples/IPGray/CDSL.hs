module Examples.IPGray.CDSL where
import Prelude hiding (Int,pi,div,foldl,map)
import QFeldspar.CDSL

redCoefficient :: Dp Word32
redCoefficient   = 30

greenCoefficient :: Dp Word32
greenCoefficient = 59

blueCoefficient :: Dp Word32
blueCoefficient  = 11

rgbToGray :: Dp Word32 -> Dp Word32 -> Dp Word32 -> Dp Word32
rgbToGray = \ r -> \ g -> \ b ->
            divE
            ((r * redCoefficient)   +
             (g * greenCoefficient) +
             (b * blueCoefficient)) 100

ipgrayVec :: Vec (Dp Word32) -> Vec (Dp Word32)
ipgrayVec = \ (Vec l f) ->
            Vec (divE l 3)
                 (\ i -> share (i * 3) (\ j ->
                         rgbToGray
                         (f j)
                         (f (j + 1))
                         (f (j + 2))))

ipgray :: Dp (Ary Word32) -> Dp (Ary Word32)
ipgray = toExpF ipgrayVec
