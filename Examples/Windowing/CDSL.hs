module Examples.Windowing.CDSL where

import Prelude hiding (Int,pi,div,foldl,map,replicate,zipWith)
import QFeldspar.CDSL
import Examples.Prelude.CDSL

windowingVec :: Vec (Dp (Complex Float)) -> Vec (Dp (Complex Float))
windowingVec = \ (Vec ll f) -> share ll (\ l ->
                      zipWith (*)
                       (append
                        (replicate (l - (divE l 4)) (1.0 :+. 0.0))
                        (replicate l                (0.0 :+. 0.0)))
                       (Vec l (\ x -> f x )))

windowing :: Dp (Ary (Complex Float)) -> Dp (Ary (Complex Float))
windowing = toExpF windowingVec
