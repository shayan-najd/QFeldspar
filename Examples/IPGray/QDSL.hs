module Examples.IPGray.QDSL where

import Prelude hiding (Int,pi,div,foldl,map)
import QFeldspar.QDSL
import Examples.Prelude.QDSL

redCoefficient :: Qt Word32
redCoefficient   = [|| 30 ||]

greenCoefficient :: Qt Word32
greenCoefficient = [|| 59 ||]

blueCoefficient :: Qt Word32
blueCoefficient  = [|| 11 ||]

rgbToGray :: Qt (Word32 -> Word32 -> Word32 -> Word32)
rgbToGray = [|| \ r -> \ g -> \ b ->
               div
               ((r * $$redCoefficient)   +
                (g * $$greenCoefficient) +
                (b * $$blueCoefficient)) 100 ||]

ipgrayVec :: Qt (Vec Word32 -> Vec Word32)
ipgrayVec = [|| \ (Vec l f) ->
                Vec (div l 3)
                    (\ i -> let j = i * 3 in
                            $$rgbToGray
                            (f j)
                            (f (j + 1))
                            (f (j + 2))) ||]

ipgray :: Qt (Ary Word32 -> Ary Word32)
ipgray = [|| \ a -> $$toArr ($$ipgrayVec ($$fromArr a)) ||]
