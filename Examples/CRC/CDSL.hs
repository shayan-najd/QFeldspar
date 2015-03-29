module Examples.CRC.CDSL where
import Prelude hiding (Int,foldl)
import QFeldspar.CDSL
import Examples.Prelude.CDSL

crcVec :: Vec (Dp Int) -> Dp Int
crcVec = foldl updCrc 0

updCrc :: Dp Int -> Dp Int -> Dp Int
updCrc = \ ccc -> \ ch -> share ccc (\ cc ->
          xorE
          (xorE
           (tbl
            ((xorE (xorE cc 0xFFFFFFFF) ch) .&.. 0xff))
           (shfRgtE (xorE cc 0xFFFFFFFF) 8))
          0xFFFFFFFF)

tbl :: Dp Int -> Dp Int
tbl = \ i -> case frmExp hashTableE of (Vec _ f) -> f i

crc :: Dp (Ary Int) -> Dp Int
crc = toExpF crcVec
