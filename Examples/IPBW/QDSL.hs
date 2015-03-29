module Examples.IPBW.QDSL where

import Prelude hiding (Int,fmap)

import QFeldspar.QDSL
import Examples.Prelude.QDSL

ipbwVec :: Qt (Vec Int -> Vec Int)
ipbwVec = [|| $$fmap (\ x -> if x < 135 then 1 else 0) ||]

ipbw :: Qt (Ary Int -> Ary Int)
ipbw = [|| \ a -> $$toArr ($$ipbwVec ($$fromArr a)) ||]
