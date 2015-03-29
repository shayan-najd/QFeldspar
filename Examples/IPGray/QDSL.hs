module Examples.IPGray.QDSL where

import Prelude hiding (Int,pi,div,foldl,map)
import QFeldspar.QDSL
import Examples.Prelude.QDSL

redCoefficient :: Qt Int
redCoefficient   = [|| 30 ||]

greenCoefficient :: Qt Int
greenCoefficient = [|| 59 ||]

blueCoefficient :: Qt Int
blueCoefficient  = [|| 11 ||]

rgbToGray :: Qt (Int -> Int -> Int -> Int)
rgbToGray = [|| \ r -> \ g -> \ b ->
               div
               ((r * $$redCoefficient)   +
                (g * $$greenCoefficient) +
                (b * $$blueCoefficient)) 100 ||]

ipgrayVec :: Qt (Vec Int -> Vec Int)
ipgrayVec = [|| \ (Vec l f) ->
                Vec (div l 3)
                    (\ i -> let j = i * 3 in
                            $$rgbToGray
                            (f j)
                            (f (j + 1))
                            (f (j + 2))) ||]

ipgray :: Qt (Ary Int -> Ary Int)
ipgray = [|| \ a -> $$toArr ($$ipgrayVec ($$fromArr a)) ||]
