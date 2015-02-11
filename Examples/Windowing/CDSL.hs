module Examples.Windowing.CDSL where
import Prelude hiding (Int,pi,div,foldl,map,replicate,zipWith)
import QFeldspar.CDSL

windowingVec :: Vec (Data Cmx) -> Vec (Data Cmx)
windowingVec = \ (Vec ll f) -> let l = $shared ll in
                      zipWith mul
                       (append
                        (replicate (sub l (div l 4)) (cmx 1.0 0.0))
                        (replicate l                (cmx 0.0 0.0)))
                       (Vec l (\ x -> f x ))

windowing :: Data (Ary Cmx) -> Data (Ary Cmx)
windowing = toExpF windowingVec
