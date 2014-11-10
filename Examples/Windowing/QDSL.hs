module Examples.Windowing.QDSL where
import Prelude hiding (Int,pi,div,foldl,map,replicate,zipWith)
import QFeldspar.QDSL

windowingVec :: Data (Vec Cmx -> Vec Cmx)
windowingVec = [|| \ (Vec ll f) -> let l = ll in
                   $$zipWith $$mul
                    ($$append
                     ($$replicate ($$sub l ($$div l 4)) (cmx 1.0 0.0))
                     ($$replicate l                     (cmx 0.0 0.0)))
                    (Vec l (\ x -> f x)) ||]

windowing :: Data (Ary Cmx -> Ary Cmx)
windowing = [|| $$toArrF $$windowingVec ||]
