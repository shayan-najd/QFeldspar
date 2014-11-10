module Examples.CRC.CDSL where
import Prelude hiding (Int,foldl)
import QFeldspar.CDSL

crcVec :: Vec (Data Int) -> Data Int
crcVec = foldl updCrc 0

updCrc :: Data Int -> Data Int -> Data Int
updCrc = \ ccc -> \ ch ->
         share ccc (\ cc ->
          bitXor
          (bitXor
           (tblV
            (bitAnd (bitXor (bitXor cc 0xFFFFFFFF) ch) 0xff))
           (shfRgt (bitXor cc 0xFFFFFFFF) 8))
          0xFFFFFFFF)

tblV :: Data Int -> Data Int
tblV = \ i -> case frmExp hashTable of (Vec _ f) -> f i

crc :: Data (Ary Int) -> Data Int
crc = toExpF crcVec
