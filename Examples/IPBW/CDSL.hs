module Examples.IPBW.CDSL where

import QFeldspar.CDSL

ipbwVec :: Vec (Data Int) -> Vec (Data Int)
ipbwVec = map (\ x -> ifThenElse (lt x 135) 1 0)

ipbw :: Data (Ary Int) -> Data (Ary Int)
ipbw = toExpF ipbwVec