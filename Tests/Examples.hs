module Tests.Examples where
{-
import QFeldspar.MyPrelude
import System.IO.Unsafe

import qualified QFeldspar.QDSL as Q
import qualified QFeldspar.CDSL as C

import qualified Examples.IPGray.QDSL as IPGrayQ
import qualified Examples.IPGray.CDSL as IPGrayC

import qualified Examples.IPBW.QDSL as IPBWQ
import qualified Examples.IPBW.CDSL as IPBWC

import qualified Examples.CRC.QDSL as CRCQ
import qualified Examples.CRC.CDSL as CRCC

import qualified Examples.FFT.QDSL as FFTQ
import qualified Examples.FFT.CDSL as FFTC

import qualified Examples.Windowing.QDSL as WindowingQ
import qualified Examples.Windowing.CDSL as WindowingC

tstIPGrayQ = Q.evaluate
             [|| $$(IPGrayQ.ipgray)
                     $$(Q.fromList (fmap Q.litI tstPPM) [|| 0 ||]) ||]

tstPPM :: [Int]
tstPPM = unsafePerformIO
         (do f <- readFile "Tests/Image/Phil/Image2.ppm"
             let "P3" : _ : "255" : c = lines f
             return (fmap (read :: String -> Int) c))
-}
{-
tstPGM :: [Int]
tstPGM = unsafePerformIO
         (do f <- readFile "Examples/Feldspar/IP/Image/Result/Phil/Image16.pgm"
             let "P2" : _ : "255" : c = lines f
             return (fmap (read :: String -> Int) c))

tstPBM :: [Int]
tstPBM = unsafePerformIO
         (do f <- readFile "Examples/Feldspar/IP/Image/Result/Phil/Image16.pbm"
             let "P1" : _ : "255" : c = lines f
             return (fmap (read :: String -> Int) c))

-}
{-
-- CRC
test :: Int -> Bool
test = (== 3632233996)

tstInp :: [Int]
tstInp = fmap (fromIntegral . ord) "test"

tstPGMW :: [Flt]
tstPGMW = unsafePerformIO
         (do f <- readFile "Examples/Feldspar/IP/Image/Result/Phil/Image16W.pgm"
             let "P1" : _ : "255" : c = lines f
             return (fmap (fromInt . (read :: String -> P.Int)) c))


tstPPM :: [Int]
tstPPM = unsafePerformIO
         (do f <- readFile "Examples/Feldspar/IP/Image/Result/Phil/Image16.ppm"
             let "P3" : _ : "255" : c = lines f
             return (fmap (read :: String -> Int) c))

tstPGM :: [Int]
tstPGM = unsafePerformIO
         (do f <- readFile "Examples/Feldspar/IP/Image/Result/Phil/Image16.pgm"
             let "P2" : _ : "255" : c = lines f
             return (fmap (read :: String -> Int) c))

tstPBM :: [Int]
tstPBM = unsafePerformIO
         (do f <- readFile "Examples/Feldspar/IP/Image/Result/Phil/Image16.pbm"
             let "P1" : _ : "255" : c = lines f
             return (fmap (read :: String -> Int) c))


-- FFT
test :: [Flt] -> Bool
test out = out ==
           [6.2831855,16.418755,8.885766,6.800871
           ,6.2831855,6.8008704,8.885766,16.418755]

tstInp :: [Flt]
tstInp = [-4.71238898038469,
          -3.141592653589793,
          -1.5707963267948966,
          0.0,
          1.5707963267948966,
          3.141592653589793,
          4.71238898038469,
          6.283185307179586]
-}