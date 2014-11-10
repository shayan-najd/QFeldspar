module Examples.IPGray.QDSL where

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
                ($$add ($$mul r $$redCoefficient )
                         ($$mul g $$greenCoefficient))
                ($$mul b $$blueCoefficient )) 100
            ||]

ipgray :: Data (Ary Int -> Ary Int)
ipgray = [|| \ v ->
              arr ($$div (arrLen v) 3)
                      (\ i -> let j = $$mul i 3 in
                              $$rgbToGray
                              (arrIx v j)
                              (arrIx v ($$add j 1))
                              (arrIx v ($$add j 2)))
         ||]
