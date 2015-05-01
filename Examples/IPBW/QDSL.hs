module Examples.IPBW.QDSL where

import Prelude hiding (Int,fmap)

import QFeldspar.QDSL
import Examples.Prelude.QDSL

ipbwVec :: Qt (Vec Word32 -> Vec Word32)
ipbwVec = [|| $$fmap (\ x -> if x < 135 then 1 else 0) ||]

ipbw :: Qt (Ary Word32 -> Ary Word32)
ipbw = [|| \ a -> $$toArr ($$ipbwVec ($$fromArr a)) ||]
