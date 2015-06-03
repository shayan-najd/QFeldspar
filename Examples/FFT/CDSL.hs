module Examples.FFT.CDSL where

import Prelude hiding (Int,pi,div)
import QFeldspar.CDSL
import Examples.Prelude.CDSL

fftVec :: Vec (Dp (Complex Float)) -> Vec (Dp (Complex Float))
fftVec = \ (Vec ll f) ->
         let l     = ll in
         let steps = (ilog2E l - 1) in
         bitRev steps (fftCore steps (Vec l (\ i -> f i)))

fftCore :: Dp Word32 -> Vec (Dp (Complex Float)) -> Vec (Dp (Complex Float))
fftCore = \ n -> \ (Vec l f) ->
          for (n + 1) (Vec l (\ i -> f i))
                (\ j -> \ v ->
                        Vec l (\ i -> ixf v (n - j) i))

ixf :: Vec (Dp (Complex Float))
    -> Dp Word32 -> Dp Word32 -> Dp (Complex Float)
ixf = \ (Vec _l f) -> \ kk -> \ i -> share kk (\ k ->
      share (shfLftE 1 k) (\ k2 ->
      share (cisE ((pi * (i2fE (lsbs k i))) / (i2fE k2))) (\ twid ->
      share (f i) (\ a ->
      share (f (xorE i k2)) (\ b ->
        (testBit i k) ?
           (twid * (b - a) , a + b))))))

bitRev :: Dp Word32 -> Vec (Dp (Complex Float)) -> Vec (Dp (Complex Float))
bitRev = \ n -> \ x ->
         for n x (\ i -> permute (\ _j -> rotBit (i + 1)))

rotBit :: Dp Word32 -> Dp Word32 -> Dp Word32
rotBit = \ kk -> \ i ->
         share kk (\ k ->
         (shfLftE ((shfLftE (shfRgtE (shfRgtE i 1) k) 1) .|..
                   (i .&.. 1)) k) .|..
         (lsbs k (shfRgtE i 1)))

fft :: Dp (Ary (Complex Float)) -> Dp (Ary (Complex Float))
fft = toExpF fftVec
