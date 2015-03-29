module Examples.Windowing.QDSL where
import Prelude hiding (Int,pi,div,foldl,map,replicate,zipWith)
import QFeldspar.QDSL
import Examples.Prelude.QDSL

windowingVec :: Qt (Vec (Complex Float) -> Vec (Complex Float))
windowingVec = [|| \ (Vec ll f) -> let l = ll in
                   $$zipWith (*)
                    ($$append
                     ($$replicate (l - (div l 4)) (1.0 :+ 0.0))
                     ($$replicate l               (0.0 :+ 0.0)))
                    (Vec l (\ x -> f x)) ||]

windowing :: Qt (Ary (Complex Float) -> Ary (Complex Float))
windowing = [|| $$toArrF $$windowingVec ||]
