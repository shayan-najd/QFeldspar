module Examples.IPBW.CDSL where
import Prelude hiding (Int)
import QFeldspar.CDSL

ipbwVec :: Vec (Data Int) -> Vec (Data Int)
ipbwVec = fmap (\ x -> (lt x 135) ? (1 , 0))

ipbw :: Data (Ary Int) -> Data (Ary Int)
ipbw = toExpF ipbwVec
