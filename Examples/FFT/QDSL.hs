module Examples.FFT.QDSL where
import QFeldspar.QDSL

fft :: Data (Ary Cmx -> Ary Cmx)
fft = [|| \ v ->
           let steps = $$sub ($$ilog2 (len v)) 1 in
           $$bitRev steps ($$fftCore steps v)
      ||]

fftCore :: Data (Int -> Ary Cmx -> Ary Cmx)
fftCore = [|| \ n -> \ vv ->
              $$forLoop ($$add n 1) vv
                      (\ j -> \ v ->
                              ary (len vv) (\ i -> $$ixf v ($$sub n j) i))
          ||]

ixf :: Data (Ary Cmx -> Int -> Int -> Cmx)
ixf = [|| \ v -> \ k -> \ i ->
          let k2   = $$shfLft 1 k in
          let twid = $$cis ($$div ($$mul $$pi ($$i2f ($$lsbs k i)))($$i2f k2)) in
          let a    = ind v i in
          let b    = ind v ($$bitXor i k2) in
            if $$testBit i k
            then $$mul twid ($$sub b a)
            else $$add a b
      ||]

bitRev :: Data (Int -> Ary Cmx -> Ary Cmx)
bitRev = [|| \ n -> \ x ->
             $$forLoop n x (\ i -> $$permute (\ _j -> $$rotBit ($$add i 1)))
         ||]

rotBit :: Data (Int -> Int -> Int)
rotBit = [|| \ k -> \ i ->
          $$bitOr
          ($$shfLft ($$bitOr
                     ($$shfLft ($$shfRgt ($$shfRgt i 1) k) 1)
                     ($$bitAnd i 1)) k)
          ($$lsbs k ($$shfRgt i 1))
         ||]
