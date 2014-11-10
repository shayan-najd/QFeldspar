module Examples.CRC.QDSL where
import Prelude hiding (Int,pi,div,foldl)
import QFeldspar.QDSL

crcVec :: Data (Vec Int -> Int)
crcVec = [|| $$foldl $$updCrc 0 ||]

updCrc :: Data (Int -> Int -> Int)
updCrc = [|| \ cc -> \ ch ->
             $$bitXor
             ($$bitXor
              ($$tbl
               ($$bitAnd ($$bitXor ($$bitXor cc 0xFFFFFFFF) ch) 0xff))
              ($$shfRgt ($$bitXor cc 0xFFFFFFFF) 8))
             0xFFFFFFFF ||]

tbl :: Data (Int -> Int)
tbl = [|| \ i -> arrIx $$hashTable i ||]

crc :: Data (Ary Int -> Int)
crc = [|| \ a -> $$crcVec ($$fromArr a) ||]
