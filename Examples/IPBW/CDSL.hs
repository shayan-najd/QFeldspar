module Examples.IPBW.CDSL where

import QFeldspar.CDSL
import Examples.Prelude.CDSL

ipbwVec :: Vec (Dp Word32) -> Vec (Dp Word32)
ipbwVec = fmap (\ x -> (x <. 135) ? (1 , 0))

ipbw :: Dp (Ary Word32) -> Dp (Ary Word32)
ipbw = toExpF ipbwVec
