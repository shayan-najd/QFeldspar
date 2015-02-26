module Examples.IPGray.QDSL where
import Prelude hiding (Int,pi,div,foldl,map)
import QFeldspar.QDSL

redCoefficient :: Data Int
redCoefficient   = [|| 30 ||]

greenCoefficient :: Data Int
greenCoefficient = [|| 59 ||]

blueCoefficient :: Data Int
blueCoefficient  = [|| 11 ||]

rgbToGray :: Data (Int -> Int -> Int -> Int)
rgbToGray = [|| \ r -> \ g -> \ b ->
               $$div
               ($$add
                ($$add (r * $$redCoefficient )
                       (g * $$greenCoefficient))
                (b * $$blueCoefficient )) 100 ||]

ipgrayVec :: Data (Vec Int -> Vec Int)
ipgrayVec = [|| \ (Vec l f) ->
                Vec ($$div l 3)
                    (\ i -> let j = i * 3 in
                            $$rgbToGray
                            (f j)
                            (f ($$add j 1))
                            (f ($$add j 2))) ||]

ipgray :: Data (Ary Int -> Ary Int)
ipgray = [|| \ a -> $$toArr ($$ipgrayVec ($$fromArr a)) ||]
