module Examples.FFT.QDSL where
import Prelude hiding (Int,pi,div)
import QFeldspar.QDSL

fftVec :: Data (Vec Cmx -> Vec Cmx)
fftVec = [|| \ (Vec ll f) ->
              let l     = ll in
              let steps = $$sub ($$ilog2 l) 1 in
              $$bitRev steps ($$fftCore steps (Vec l (\ i -> f i))) ||]

fftCore :: Data (Int -> Vec Cmx -> Vec Cmx)
fftCore = [|| \ n -> \ (Vec l f) ->
              $$forVec ($$add n 1) (Vec l (\ i -> f i))
                      (\ j -> \ v ->
                         Vec l (\ i -> $$ixf v ($$sub n j) i)) ||]

ixf :: Data (Vec Cmx -> Int -> Int -> Cmx)
ixf = [|| \ (Vec _l f) -> \ k -> \ i ->
          let k2   = $$shfLft 1 k in
          let twid = $$cis ($$div ($$pi *
                                   ($$i2f ($$lsbs k i)))($$i2f k2)) in
          let a    = f i in
          let b    = f ($$bitXor i k2) in
            if $$testBit i k then $$mul twid ($$sub b a)
                             else $$add a b ||]

bitRev :: Data (Int -> Vec Cmx -> Vec Cmx)
bitRev = [|| \ n -> \ x ->
             $$forVec n x
              (\ i -> $$permute (\ _j -> $$rotBit ($$add i 1))) ||]

rotBit :: Data (Int -> Int -> Int)
rotBit = [|| \ k -> \ i ->
          $$bitOr
          ($$shfLft ($$bitOr
                     ($$shfLft ($$shfRgt ($$shfRgt i 1) k) 1)
                     ($$bitAnd i 1)) k)
          ($$lsbs k ($$shfRgt i 1)) ||]

fft :: Data (Ary Cmx -> Ary Cmx)
fft = [|| \ a -> $$toArr ($$fftVec ($$fromArr a)) ||]
