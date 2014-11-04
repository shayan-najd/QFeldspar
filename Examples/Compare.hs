module Examples.Compare where

import Prelude (Bool(..),(&&),Int)
import qualified QFeldspar.QDSL as Q
import qualified QFeldspar.CDSL as C

import qualified Examples.IPBW.QDSL as IPBWQ
import qualified Examples.IPBW.CDSL as IPBWC

import qualified Examples.IPGray.QDSL as IPGrayQ
import qualified Examples.IPGray.CDSL as IPGrayC

import qualified Examples.FFT.QDSL as FFTQ
import qualified Examples.FFT.CDSL as FFTC

import qualified Examples.CRC.QDSL as CRCQ
import qualified Examples.CRC.CDSL as CRCC

import qualified Examples.Windowing.QDSL as WQ
import qualified Examples.Windowing.CDSL as WC

import qualified Examples.Power.QDSL as PQ
import qualified Examples.Power.CDSL as PC


import QFeldspar.Expression.Feldspar.MiniFeldspar
import QFeldspar.Nat.TH
import QFeldspar.Variable.Typed
import QFeldspar.Environment.Typed

qIPBW :: C.Data (C.Ary C.Int) -> C.Data (C.Ary C.Int)
qIPBW = C.simplifyF (C.normaliseF True (Q.translateF IPBWQ.ipbw))

cIPBW :: C.Data (C.Ary C.Int) -> C.Data (C.Ary C.Int)
cIPBW = C.simplifyF (C.normaliseF True IPBWC.ipbw)

qIPGray :: C.Data (C.Ary C.Int) -> C.Data (C.Ary C.Int)
qIPGray = C.simplifyF (C.normaliseF True (Q.translateF IPGrayQ.ipgray))

cIPGray :: C.Data (C.Ary C.Int) -> C.Data (C.Ary C.Int)
cIPGray = C.simplifyF (C.normaliseF True IPGrayC.ipgray)

qFFT :: C.Data (C.Ary C.Cmx) -> C.Data (C.Ary C.Cmx)
qFFT = C.simplifyF (C.normaliseF True (Q.translateF FFTQ.fft))

cFFT :: C.Data (C.Ary C.Cmx) -> C.Data (C.Ary C.Cmx)
cFFT = C.simplifyF (C.normaliseF True FFTC.fft)

qCRC :: C.Data (C.Ary C.Int) -> C.Data C.Int
qCRC = C.simplifyF (C.normaliseF True (Q.translateF CRCQ.crc))

cCRC :: C.Data (C.Ary C.Int) -> C.Data C.Int
cCRC = C.simplifyF (C.normaliseF True CRCC.crc)

qW :: C.Data (C.Ary C.Cmx) -> C.Data (C.Ary C.Cmx)
qW = C.simplifyF (C.normaliseF True (Q.translateF WQ.windowing))

cW :: C.Data (C.Ary C.Cmx) -> C.Data (C.Ary C.Cmx)
cW = C.simplifyF (C.normaliseF True WC.windowing)

qP :: Int -> C.Data C.Flt -> C.Data C.Flt
qP n = C.simplifyF (C.normaliseF True (Q.translateF (PQ.power n)))

cP :: Int -> C.Data C.Flt -> C.Data C.Flt
cP n = C.simplifyF (C.normaliseF True (PC.power n))

qP' :: Int -> C.Data C.Flt -> C.Data C.Flt
qP' n = C.simplifyF (C.normaliseF True (Q.translateF (PQ.power'' n)))

cP' :: Int -> C.Data C.Flt -> C.Data C.Flt
cP' n = C.simplifyF (C.normaliseF True (PC.power'' n))

result :: Bool
result = eqlF qIPBW      cIPBW      &&
         eqlF qIPGray    cIPGray    &&
--         eqlF qFFT     cFFT       &&
--         eqlF qFFT       cfft       &&
         eqlF qCRC       cCRC       &&
         eqlF qW         cW         &&
         eqlF (qP 2)     (cP 2)     &&
         eqlF (qP (-2))  (cP (-2))  &&
         eqlF (qP 1)     (cP 1)     &&
         eqlF (qP (-1))  (cP (-1))  &&
         eqlF (qP' 2)    (cP' 2)    &&
         eqlF (qP' (-2)) (cP' (-2)) &&
         eqlF (qP' 1)    (cP' 1)    &&
         eqlF (qP' (-1)) (cP' (-1))

qfft :: C.Data (C.Ary C.Cmx) -> C.Data (C.Ary C.Cmx)
qfft =
 (\ x1 ->
 (Let (AppV $(nat 9 "") (Ext (AppV $(nat 28 "") (Ext (Len (x1)) Emp)) (Ext (ConI 1) Emp)))
 (\ x12805 -> (Snd (Whl
 (\ x12806 -> (AppV $(nat 6 "") (Ext (Fst (x12806)) (Ext (x12805) Emp))))
 (\ x12807 ->
 (Let (Fst (x12807))
 (\ x12808 ->
 (Let (Snd (x12807))
 (\ x12809 -> (Tpl (AppV $(nat 8 "") (Ext (x12808) (Ext (ConI 1) Emp))) (Ary (Len (x12809))
 (\ x12810 ->
 (Let (AppV $(nat 8 "") (Ext (x12808) (Ext (ConI 1) Emp)))
 (\ x12811 -> (Ind (x12809) (AppV $(nat 21 "") (Ext (AppV $(nat 24 "") (Ext (AppV $(nat 21 "") (Ext (AppV $(nat 24 "") (Ext (AppV $(nat 23 "") (Ext (AppV $(nat 23 "") (Ext (x12810) (Ext (ConI 1) Emp))) (Ext (x12811) Emp))) (Ext (ConI 1) Emp))) (Ext (AppV $(nat 20 "") (Ext (x12810) (Ext (ConI 1) Emp))) Emp))) (Ext (x12811) Emp))) (Ext (AppV $(nat 20 "") (Ext (AppV $(nat 23 "") (Ext (x12810) (Ext (ConI 1) Emp))) (Ext (AppV $(nat 25 "") (Ext (AppV $(nat 24 "") (Ext (AppV $(nat 25 "") (Ext (ConI 0) Emp)) (Ext (x12811) Emp))) Emp)) Emp))) Emp)))))))))))))) (Tpl (ConI 0) (Snd (Whl
 (\ x12812 -> (AppV $(nat 6 "") (Ext (Fst (x12812)) (Ext (AppV $(nat 8 "") (Ext (x12805) (Ext (ConI 1) Emp))) Emp))))
 (\ x12813 ->
 (Let (Fst (x12813))
 (\ x12814 ->
 (Let (Snd (x12813))
 (\ x12815 -> (Tpl (AppV $(nat 8 "") (Ext (x12814) (Ext (ConI 1) Emp))) (Ary (Len (x1))
 (\ x12816 ->
 (Let (AppV $(nat 9 "") (Ext (x12805) (Ext (x12814) Emp)))
 (\ x12817 ->
 (Let (AppV $(nat 24 "") (Ext (ConI 1) (Ext (x12817) Emp)))
 (\ x12818 ->
 (Let (Ind (x12815) (x12816))
 (\ x12819 ->
 (Let (Ind (x12815) (AppV $(nat 22 "") (Ext (x12816) (Ext (x12818) Emp))))
 (\ x12820 -> (Cnd (AppV $(nat 3 "") (Ext (AppV $(nat 20 "") (Ext (x12816) (Ext (AppV $(nat 24 "") (Ext (ConI 1) (Ext (x12817) Emp))) Emp))) (Ext (ConI 0) Emp))) (AppV $(nat 16 "") (Ext (x12819) (Ext (x12820) Emp))) (AppV $(nat 18 "") (Ext (AppV $(nat 27 "") (Ext (AppV $(nat 15 "") (Ext (AppV $(nat 14 "") (Ext (ConF (-3.1415927)) (Ext (AppV $(nat 26 "") (Ext (AppV $(nat 20 "") (Ext (x12816) (Ext (AppV $(nat 25 "") (Ext (AppV $(nat 24 "") (Ext (AppV $(nat 25 "") (Ext (ConI 0) Emp)) (Ext (x12817) Emp))) Emp)) Emp))) Emp)) Emp))) (Ext (AppV $(nat 26 "") (Ext (x12818) Emp)) Emp))) Emp)) (Ext (AppV $(nat 17 "") (Ext (x12820) (Ext (x12819) Emp))) Emp)))))))))))))))))))) (Tpl (ConI 0) (x1))))))))))

cfft :: C.Data (C.Ary C.Cmx) -> C.Data (C.Ary C.Cmx)
cfft =
 (\ x1 ->
 (Let (AppV $(nat 9 "") (Ext (AppV $(nat 28 "") (Ext (Len (x1)) Emp)) (Ext (ConI 1) Emp)))
 (\ x6776 -> (Snd (Whl
 (\ x6777 -> (AppV $(nat 6 "") (Ext (Fst (x6777)) (Ext (x6776) Emp))))
 (\ x6778 ->
 (Let (Fst (x6778))
 (\ x6779 ->
 (Let (Snd (x6778))
 (\ x6780 -> (Tpl (AppV $(nat 8 "") (Ext (x6779) (Ext (ConI 1) Emp))) (Ary (Len (x6780))
 (\ x6781 ->
 (Let (AppV $(nat 8 "") (Ext (x6779) (Ext (ConI 1) Emp)))
 (\ x6782 -> (Ind (x6780) (AppV $(nat 21 "") (Ext (AppV $(nat 24 "") (Ext (AppV $(nat 21 "") (Ext (AppV $(nat 24 "") (Ext (AppV $(nat 23 "") (Ext (AppV $(nat 23 "") (Ext (x6781) (Ext (ConI 1) Emp))) (Ext (x6782) Emp))) (Ext (ConI 1) Emp))) (Ext (AppV $(nat 20 "") (Ext (x6781) (Ext (ConI 1) Emp))) Emp))) (Ext (x6782) Emp))) (Ext (AppV $(nat 20 "") (Ext (AppV $(nat 23 "") (Ext (x6781) (Ext (ConI 1) Emp))) (Ext (AppV $(nat 25 "") (Ext (AppV $(nat 24 "") (Ext (AppV $(nat 25 "") (Ext (ConI 0) Emp)) (Ext (x6782) Emp))) Emp)) Emp))) Emp)))))))))))))) (Tpl (ConI 0) (Snd (Whl
 (\ x6783 -> (AppV $(nat 6 "") (Ext (Fst (x6783)) (Ext (AppV $(nat 8 "") (Ext (x6776) (Ext (ConI 1) Emp))) Emp))))
 (\ x6784 ->
 (Let (Fst (x6784))
 (\ x6785 ->
 (Let (Snd (x6784))
 (\ x6789 -> (Tpl (AppV $(nat 8 "") (Ext (x6785) (Ext (ConI 1) Emp))) (Ary (Len (x1))
 (\ x6786 ->
 (Let (AppV $(nat 9 "") (Ext (x6776) (Ext (x6785) Emp)))
 (\ x6787 ->
 (Let (AppV $(nat 24 "") (Ext (ConI 1) (Ext (x6787) Emp)))
 (\ x6788 ->
 (Let (Ind (x6789) (x6786))
 (\ x6790 ->
 (Let (Ind (x6789) (AppV $(nat 22 "") (Ext (x6786) (Ext (x6788) Emp))))
 (\ x6791 -> (Cnd (AppV $(nat 3 "") (Ext (AppV $(nat 20 "") (Ext (x6786) (Ext (AppV $(nat 24 "") (Ext (ConI 1) (Ext (x6787) Emp))) Emp))) (Ext (ConI 0) Emp))) (AppV $(nat 16 "") (Ext (x6790) (Ext (x6791) Emp))) (AppV $(nat 18 "") (Ext (AppV $(nat 27 "") (Ext (AppV $(nat 15 "") (Ext (AppV $(nat 14 "") (Ext (ConF (-3.1415927)) (Ext (AppV $(nat 26 "") (Ext (AppV $(nat 20 "") (Ext (x6786) (Ext (AppV $(nat 25 "") (Ext (AppV $(nat 24 "") (Ext (AppV $(nat 25 "") (Ext (ConI 0) Emp)) (Ext (x6787) Emp))) Emp)) Emp))) Emp)) Emp))) (Ext (AppV $(nat 26 "") (Ext (x6788) Emp)) Emp))) Emp)) (Ext (AppV $(nat 17 "") (Ext (x6791) (Ext (x6790) Emp))) Emp)))))))))))))))))))) (Tpl (ConI 0) (x1))))))))))