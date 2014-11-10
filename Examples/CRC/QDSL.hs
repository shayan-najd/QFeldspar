module Examples.CRC.QDSL where

import QFeldspar.QDSL

crc :: Data (Ary Int -> Int)
crc = [|| $$foldl $$updCrc 0 ||]

updCrc :: Data (Int -> Int -> Int)
updCrc = [|| \ cc -> \ ch ->
             $$bitXor
             ($$bitXor
              (arrIx $$tbl
               ($$bitAnd ($$bitXor ($$bitXor cc 0xFFFFFFFF) ch) 0xff))
              ($$shfRgt ($$bitXor cc 0xFFFFFFFF) 8))
             0xFFFFFFFF ||]

tbl :: Data (Ary Int)
tbl = hashTable
