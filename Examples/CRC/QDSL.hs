module Examples.CRC.QDSL where

import Prelude hiding (Int,div,foldl)
import QFeldspar.QDSL
import Examples.Prelude.QDSL

crcVec :: Qt (Vec Int -> Int)
crcVec = [|| $$foldl $$updCrc 0 ||]

updCrc :: Qt (Int -> Int -> Int)
updCrc = [|| \ cc -> \ ch ->
             xor
             (xor
              ($$tbl
               ((xor (xor cc 0xFFFFFFFF) ch) .&. 0xff))
              (shfRgt (xor cc 0xFFFFFFFF) 8))
             0xFFFFFFFF ||]

tbl :: Qt (Int -> Int)
tbl = [|| \ i -> ixArr hashTable i ||]

crc :: Qt (Ary Int -> Int)
crc = [|| \ a -> $$crcVec ($$fromArr a) ||]
