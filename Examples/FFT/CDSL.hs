module Examples.FFT.CDSL where

import QFeldspar.CDSL

fftVec :: Vec (Data Cmx) -> Vec (Data Cmx)
fftVec = \ v ->
         let steps = shared (sub (ilog2 (len v)) 1) in
         bitRev steps (fftCore steps v)

fftCore :: Data Int -> Vec (Data Cmx) -> Vec (Data Cmx)
fftCore = \ n -> \ vv ->
          forLoop (add n 1) vv
                (\ j -> \ v ->
                        vec (len vv) (\ i -> ixf v (sub n j) i))

ixf :: Vec (Data Cmx)
    -> Data Int -> Data Int -> Data Cmx
ixf = \ v -> \ kk -> \ i ->
      share kk (\ k ->

      share (shfLft 1 k) (\ k2 ->
      share (cis ((mul pi (i2f (lsbs k i))) / (i2f k2))) (\ twid ->
      share (ind v i) (\ a ->
      share (ind v (bitXor i k2)) (\ b ->
        ifThenElse (testBit i k) (mul twid (sub b a)) (add a b))))))

bitRev :: Data Int -> Vec (Data Cmx) -> Vec (Data Cmx)
bitRev = \ n -> \ x ->
         forLoop n x (\ i -> permute (\ _j -> rotBit (add i 1)))

rotBit :: Data Int -> Data Int -> Data Int
rotBit = \ kk -> \ i ->
         share kk (\ k ->

         bitOr
         (shfLft (bitOr
                  (shfLft (shfRgt (shfRgt i 1) k) 1)
                  (bitAnd i 1)) k)
         (lsbs k (shfRgt i 1)))

fft :: Data (Ary Cmx) -> Data (Ary Cmx)
fft = toExpF fftVec