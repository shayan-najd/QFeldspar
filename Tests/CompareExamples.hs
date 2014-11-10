module Tests.CompareExamples where

import Prelude (Bool(..),(&&),Int,Float)
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

qP :: Int -> C.Data Float -> C.Data Float
qP n = C.simplifyF (C.normaliseF True (Q.translateF (PQ.power n)))

cP :: Int -> C.Data Float -> C.Data Float
cP n = C.simplifyF (C.normaliseF True (PC.power n))

qP' :: Int -> C.Data Float -> C.Data Float
qP' n = C.simplifyF (C.normaliseF True (Q.translateF (PQ.power'' n)))

cP' :: Int -> C.Data Float -> C.Data Float
cP' n = C.simplifyF (C.normaliseF True (PC.power'' n))

result :: Bool
result = C.trmEqlF qIPBW      cIPBW      &&
         C.trmEqlF qIPGray    cIPGray    &&
         C.trmEqlF qFFT       cFFT       &&
         C.trmEqlF qCRC       cCRC       &&
         C.trmEqlF qW         cW         &&
         C.trmEqlF (qP 2)     (cP 2)     &&
         C.trmEqlF (qP (-2))  (cP (-2))  &&
         C.trmEqlF (qP 1)     (cP 1)     &&
         C.trmEqlF (qP (-1))  (cP (-1))  &&
         C.trmEqlF (qP 0)     (cP 0)     &&
         C.trmEqlF (qP' 2)    (cP' 2)    &&
         C.trmEqlF (qP' (-2)) (cP' (-2)) &&
         C.trmEqlF (qP' 1)    (cP' 1)    &&
         C.trmEqlF (qP' (-1)) (cP' (-1)) &&
         C.trmEqlF (qP' 0)    (cP' 0)
