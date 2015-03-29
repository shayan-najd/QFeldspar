module Examples.IPBW.CDSL where

import QFeldspar.CDSL
import Examples.Prelude.CDSL

ipbwVec :: Vec (Dp Int) -> Vec (Dp Int)
ipbwVec = fmap (\ x -> (x <. 135) ? (1 , 0))

ipbw :: Dp (Ary Int) -> Dp (Ary Int)
ipbw = toExpF ipbwVec
