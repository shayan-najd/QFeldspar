module Examples.CRC.QDSL where

import Prelude hiding (Int,div,foldl)
import QFeldspar.QDSL
import Examples.Prelude.QDSL

crcVec :: Qt (Vec Word32 -> Word32)
crcVec = [|| $$foldl $$updCrc 0 ||]

updCrc :: Qt (Word32 -> Word32 -> Word32)
updCrc = [|| \ cc -> \ ch ->
             xor
             (xor
              ($$tbl
               ((xor (xor cc 0xFFFFFFFF) ch) .&. 0xff))
              (shfRgt (xor cc 0xFFFFFFFF) 8))
             0xFFFFFFFF ||]

tbl :: Qt (Word32 -> Word32)
tbl = [|| \ i -> ixArr hashTable i ||]

crc :: Qt (Ary Word32 -> Word32)
crc = [|| \ a -> $$crcVec ($$fromArr a) ||]
