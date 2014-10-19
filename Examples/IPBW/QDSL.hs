module Examples.IPBW.QDSL where

import QFeldspar.QDSL

ipbw :: Data (Ary Int -> Ary Int)
ipbw = [|| $$map (\ x -> if $$lt x 135 then 1 else 0) ||]