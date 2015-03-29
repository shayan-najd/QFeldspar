module Examples.FFT.QDSL where

import Prelude hiding (Int,pi,div)
import QFeldspar.QDSL
import Examples.Prelude.QDSL

fftVec :: Qt (Vec (Complex Float) -> Vec (Complex Float))
fftVec = [|| \ (Vec ll f) ->
              let l     = ll in
              let steps = ilog2 l - 1 in
              $$bitRev steps ($$fftCore steps (Vec l (\ i -> f i))) ||]

fftCore :: Qt (Int -> Vec (Complex Float) -> Vec (Complex Float))
fftCore = [|| \ n -> \ (Vec l f) ->
              $$forVec (n + 1) (Vec l (\ i -> f i))
                      (\ j -> \ v ->
                         Vec l (\ i -> $$ixf v (n - j) i)) ||]

ixf :: Qt (Vec (Complex Float) -> Int -> Int -> (Complex Float))
ixf = [|| \ (Vec _l f) -> \ k -> \ i ->
          let k2   = shfLft 1 k in
          let twid = cis (($$pi *
                           (i2f ($$lsbs k i))) / (i2f k2)) in
          let a    = f i in
          let b    = f (xor i k2) in
            if $$testBit i k then twid * (b - a)
                             else a + b ||]

bitRev :: Qt (Int -> Vec (Complex Float) -> Vec (Complex Float))
bitRev = [|| \ n -> \ x ->
             $$forVec n x
              (\ i -> $$permute (\ _j -> $$rotBit (i + 1))) ||]

rotBit :: Qt (Int -> Int -> Int)
rotBit = [|| \ k -> \ i ->
          (shfLft ((shfLft (shfRgt (shfRgt i 1) k) 1) .|.
                  (i .&. 1)) k) .|.
          ($$lsbs k (shfRgt i 1)) ||]

fft :: Qt (Ary (Complex Float) -> Ary (Complex Float))
fft = [|| \ a -> $$toArr ($$fftVec ($$fromArr a)) ||]
