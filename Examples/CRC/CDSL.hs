module Examples.CRC.CDSL where
import Prelude hiding (Int,foldl)
import QFeldspar.CDSL
import Examples.Prelude.CDSL

crcVec :: Vec (Dp Word32) -> Dp Word32
crcVec = foldl updCrc 0

updCrc :: Dp Word32 -> Dp Word32 -> Dp Word32
updCrc = \ ccc -> \ ch -> share ccc (\ cc ->
          xorE
          (xorE
           (tbl
            ((xorE (xorE cc 0xFFFFFFFF) ch) .&.. 0xff))
           (shfRgtE (xorE cc 0xFFFFFFFF) 8))
          0xFFFFFFFF)

tbl :: Dp Word32 -> Dp Word32
tbl = \ i -> case frmExp hashTableE of (Vec _ f) -> f i

crc :: Dp (Ary Word32) -> Dp Word32
crc = toExpF crcVec
