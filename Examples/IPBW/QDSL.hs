module Examples.IPBW.QDSL where
import Prelude hiding (Int,fmap)
import QFeldspar.QDSL

ipbwVec :: Data (Vec Int -> Vec Int)
ipbwVec = [|| $$fmap (\ x -> if $$lt x 135 then 1 else 0) ||]

ipbw :: Data (Ary Int -> Ary Int)
ipbw = [|| \ a -> $$toArr ($$ipbwVec ($$fromArr a)) ||]
