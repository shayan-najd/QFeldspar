module Examples.FFT.CDSL where
import Prelude hiding (Int,pi,div)
import QFeldspar.CDSL

fftVec :: Vec (Data Cmx) -> Vec (Data Cmx)
fftVec = \ (Vec ll f) ->
         let l     = shared ll in
         let steps = shared (sub (ilog2 l) 1) in
         bitRev steps (fftCore steps (Vec l (\ i -> f i)))

fftCore :: Data Int -> Vec (Data Cmx) -> Vec (Data Cmx)
fftCore = \ n -> \ (Vec l f) ->
          for (add n 1) (Vec l (\ i -> f i))
                (\ j -> \ v ->
                        Vec l (\ i -> ixf v (sub n j) i))

ixf :: Vec (Data Cmx)
    -> Data Int -> Data Int -> Data Cmx
ixf = \ (Vec _l f) -> \ kk -> \ i ->
      share kk (\ k ->
      share (shfLft 1 k) (\ k2 ->
      share (cis ((mul pi (i2f (lsbs k i))) / (i2f k2))) (\ twid ->
      share (f i) (\ a ->
      share (f (bitXor i k2)) (\ b ->
        (testBit i k) ? (mul twid (sub b a) , add a b))))))

bitRev :: Data Int -> Vec (Data Cmx) -> Vec (Data Cmx)
bitRev = \ n -> \ x ->
         for n x (\ i -> permute (\ _j -> rotBit (add i 1)))

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
