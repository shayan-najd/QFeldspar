module Examples.FFT.CDSL where

import QFeldspar.CDSL

fftVec :: Vec Cmx -> Vec Cmx
fftVec = \ v ->
         let steps = shared (sub (ilog2 (lenV v)) 1) in
         bitRev steps (fftCore steps v)

fftCore :: Data Int -> Vec Cmx -> Vec Cmx
fftCore = \ n -> \ vv ->
          forLoopVec (add n 1) vv
                (\ j -> \ v ->
                        vec (lenV vv) (\ i -> ixf v (sub n j) i))

ixf :: Vec Cmx
    -> Data Int -> Data Int -> Data Cmx
ixf = \ v -> \ kk -> \ i ->
      let k    = shared  kk in

      let k2   = shared (shfLft 1 k) in
      let twid = shared (cis ((mul pi (i2f (lsbs k i))) / (i2f k2))) in
      let a    = shared (indV v i) in
      let b    = shared (indV v (bitXor i k2)) in
        ifThenElse (testBit i k) (mul twid (sub b a)) (add a b)

bitRev :: Data Int -> Vec Cmx -> Vec Cmx
bitRev = \ n -> \ x ->
         forLoopVec n x (\ i -> permute (\ _j -> rotBit (add i 1)))

rotBit :: Data Int -> Data Int -> Data Int
rotBit = \ kk -> \ i ->
         let k = shared kk in

         bitOr
         (shfLft (bitOr
                  (shfLft (shfRgt (shfRgt i 1) k) 1)
                  (bitAnd i 1)) k)
         (lsbs k (shfRgt i 1))

fft :: Data (Ary Cmx) -> Data (Ary Cmx)
fft a  = vec2ary (fftVec (ary2vec a))