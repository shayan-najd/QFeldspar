module Examples.Prelude.CDSL
  (frmTo,permute,reverse,foldl,zipWith,sum,
  {-forVec,toArr,fromArr,toArrF,-}
   scalarProd,P.fmap,replicate,append,for,not,and,or,notEql,gt,
   lte,gte,min,pi,testBit,lsbs,oneBits) where

import qualified Prelude as P
import QFeldspar.CDSL

frmTo :: Dp Word32 -> Dp Word32 -> Vec (Dp Word32)
frmTo = \ mm -> \ nn -> let n = $shared nn in
                        let m = $shared mm in
                        Vec
                        ((n <. m) ?
                         (0 ,
                          n - m + 1))
                        (\ i -> i + m)

permute :: (Dp Word32 -> Dp Word32 -> Dp Word32)
           -> Vec t -> Vec t
permute = \ f -> \ (Vec l g) -> let lv = $shared l in
                                Vec lv (\ i -> g (f lv i))

reverse :: Vec t -> Vec t
reverse = permute (\ l -> \ i -> l - 1 - i)

foldl :: (Type (InT a) , Syn a , Syn b) =>
         (a -> b -> a) -> a -> Vec b -> a
foldl = \ f -> \ acc -> \ (Vec l g) ->
        for l acc (\ i -> \ a -> f a (g i))

zipWith :: (Syn a , Syn b , Syn c) =>
           (a -> b -> c) -> Vec a -> Vec b -> Vec c
zipWith = \ f -> \ (Vec l1 g1) -> \ (Vec l2 g2) ->
          Vec (min l1 l2)
              (\ ii -> share ii (\ i ->
                       f (g1 i) (g2 i)))

sum :: (Syn a , Num a , Type (InT a)) => Vec a -> a
sum = foldl (+) 0

scalarProd :: Vec (Dp Word32) -> Vec (Dp Word32) -> Dp Word32
scalarProd = \ v1 -> \ v2 -> sum (zipWith (*) v1 v2)

instance P.Functor Vec where
    fmap f (Vec n g) = Vec n (f . g)

replicate :: Dp Word32 -> a -> Vec a
replicate = \ n -> \ x -> Vec n (\ _i -> x)

append :: Syn a => Vec a -> Vec a -> Vec a
append  = \ (Vec l1 f1) -> \ (Vec l2 f2) ->
          let lv1 = $shared l1 in
          Vec (lv1 + l2)
              (\ ii -> share ii (\ i ->
                       (i <. lv1) ?
                         (f1 i ,
                          f2 i)))

for :: (Type (InT a) , Syn a) =>
           Dp Word32 -> a -> (Dp Word32 -> a -> a) -> a
for l init step = snd (while (\ t -> fst t <. l)
                             (\ tt -> share tt (\ t ->
                                      share (fst t) (\ ft ->
                                      (ft + 1 , step ft (snd t)))))
                               (0 , init))

not :: Dp Bool -> Dp Bool
not = \ x -> x ? (FalseE , TrueE)

and :: Dp Bool -> Dp Bool -> Dp Bool
and = \ x -> \ y -> x ? (y , FalseE)

or :: Dp Bool -> Dp Bool -> Dp Bool
or = \ x -> \ y -> x ? (TrueE , y)

notEql :: EqE t => Dp t -> Dp t -> Dp Bool
notEql = \ x -> \ y -> not (x ==. y)

gt :: (EqE t , OrdE t , Type t) =>
      Dp t -> Dp t -> Dp Bool
gt =  \ xx -> \ yy -> share xx (\ x ->
                      share yy (\ y ->
                      not (or (x <. y) (x ==. y))))

lte :: (EqE t , OrdE t , Type t) =>
       Dp t -> Dp t -> Dp Bool
lte = \ xx -> \ yy -> share xx (\ x ->
                      share yy (\ y ->
                      or (x <. y) (x ==. y)))

gte :: (EqE t , OrdE t) => Dp t -> Dp t -> Dp Bool
gte = \ x -> \ y -> not (x <. y)

min :: (OrdE t , Type t) => Dp t -> Dp t -> Dp t
min xx yy = share xx (\ x ->
            share yy (\ y ->
            (x <. y) ? (x , y)))

pi :: Dp Float
pi = conF ((P.negate P.pi) :: Float)

testBit    :: Dp Word32 -> Dp Word32 -> Dp Bool
testBit    = \ i -> \ j -> ((i .&.. (shfLftE 1 j)) ==. 0) ?
                           (FalseE
                           ,TrueE)

lsbs :: Dp Word32 -> Dp Word32 -> Dp Word32
lsbs       = \ k -> \ i ->  i .&.. (oneBits k)

oneBits :: Dp Word32 -> Dp Word32
oneBits    = \ n -> complementE (shfLftE (complementE 0) n)
