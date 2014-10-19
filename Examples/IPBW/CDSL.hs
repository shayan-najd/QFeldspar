module Examples.IPBW.CDSL where

import QFeldspar.CDSL

ipbwVec :: Vec Int -> Vec Int
ipbwVec = map (\ x -> ifThenElse (lt x 135) 1 0)

ipbw :: Data (Ary Int) -> Data (Ary Int)
ipbw a = vec2ary (ipbwVec (ary2vec a))