module Examples.Windowing.QDSL where

import QFeldspar.QDSL

windowing :: Data (Ary Cmx -> Ary Cmx)
windowing = [|| \ s -> let l = arrLen s in
                       $$zipWith $$mul
                        ($$append
                         ($$replicate ($$sub l ($$div l 4)) (cmx 1.0 0.0))
                         ($$replicate l                     (cmx 0.0 0.0))) s ||]
