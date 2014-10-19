module Examples.Windowing.CDSL where

import QFeldspar.CDSL

windowingVec :: Vec Cmx -> Vec Cmx
windowingVec s = let l = shared (lenV s)
              in  zipWith mul (append
                               (replicate (sub l (div l 4))
                                              (cmx 1.0 0.0))
                               (replicate l (cmx 0.0 0.0))) s

windowing :: Data (Ary Cmx) -> Data (Ary Cmx)
windowing a = vec2ary (windowingVec (ary2vec a))