module Examples.CRC.CDSL where

import QFeldspar.CDSL

crcVec :: Vec Int -> Data Int
crcVec = foldl updCrc 0

updCrc :: Data Int -> Data Int -> Data Int
updCrc = \ cc -> \ ch ->
          bitXor
          (bitXor
           (indV tblV
            (bitAnd (bitXor (bitXor cc 0xFFFFFFFF) ch) 0xff))
           (shfRgt (bitXor cc 0xFFFFFFFF) 8))
          0xFFFFFFFF

tblV :: Vec Int
tblV = ary2vec hashTable

crc :: Data (Ary Int) -> Data Int
crc a = crcVec (ary2vec a)
