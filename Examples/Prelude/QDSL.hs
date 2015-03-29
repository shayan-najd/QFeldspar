module Examples.Prelude.QDSL
  (toArr,fromArr,toArrF,frmTo,permute,reverse,foldl,zipWith,sum,
   scalarProd,fmap,forVec,replicate,append,for,not,and,or,notEql,gt,
   lte,gte,min,pi,testBit,lsbs,oneBits) where

import qualified Prelude as P
import QFeldspar.QDSL

toArr        ::  Qt (Vec a -> Ary a)
toArr        =   [|| \(Vec l g) -> mkArr l (\ i -> g i) ||]

fromArr      ::  Qt (Ary a -> Vec a)
fromArr      =   [|| \ aa -> let a = aa in
                             Vec (lnArr a) (\i -> ixArr a i) ||]

toArrF ::  Qt ((Vec a -> Vec b) -> Ary a -> Ary b)
toArrF =   [|| \ f a -> $$toArr (f ($$fromArr a)) ||]

frmTo :: Qt (Int -> Int -> Vec Int)
frmTo = [|| \ mm -> \ nn -> let n = nn in
                            let m = mm in
                            Vec
                            (if n < m
                             then 0
                             else n - m + 1)
                            (\ i -> i + m) ||]

permute :: FO t => Qt ((Int -> Int -> Int) -> Vec t -> Vec t)
permute = [|| \ f -> \ (Vec l g) -> let lv = l in
                            Vec lv (\ i -> g (f lv i)) ||]

reverse :: FO t => Qt (Vec t -> Vec t)
reverse = [|| $$permute (\ l -> \ i ->  l - 1 - i) ||]

foldl :: (FO a , FO b) => Qt ((a -> b -> a) -> a -> Vec b -> a)
foldl = [|| \ f -> \ acc -> \ (Vec l g) ->
            $$for l acc (\ i -> \ a -> f a (g i)) ||]

zipWith :: (FO a , FO b , FO c) =>
           Qt ((a -> b -> c) -> Vec a -> Vec b -> Vec c)
zipWith = [|| \ f -> \ (Vec l1 g1) -> \ (Vec l2 g2) ->
                Vec ($$min l1 l2)
                    (\ ii -> let i = ii in
                             f (g1 i) (g2 i)) ||]

sum :: Qt (Vec Int -> Int)
sum = [|| $$foldl (+) 0 ||]

scalarProd :: Qt (Vec Int -> Vec Int -> Int)
scalarProd  = [|| \ v1 -> \ v2 -> $$sum ($$zipWith (*) v1 v2) ||]

fmap :: (FO a , FO b) => Qt ((a -> b) -> Vec a -> Vec b)
fmap = [|| \ f -> \ (Vec l g) -> Vec l (\ i -> f (g i)) ||]

forVec :: FO s =>
          Qt (Int -> Vec s -> (Int -> Vec s -> Vec s) -> Vec s)
forVec = [|| \ l -> \ init -> \ step ->
             let init' = $$toArr init in
             let step' = \ i a -> $$toArr (step i ($$fromArr a)) in
             $$fromArr (snd (while
                             (\ t  -> fst t < l)
                             (\ tt -> let t  = tt    in
                                      let ft = fst t in
                                      ( ft + 1
                                      , step' ft (snd t)))
                             (0 , init')))
          ||]

replicate :: FO a => Qt (Int -> a -> Vec a)
replicate = [|| \ n -> \ x -> Vec n (\ _i -> x) ||]

append :: FO a => Qt (Vec a -> Vec a -> Vec a)
append = [|| \ (Vec l1 f1) -> \ (Vec l2 f2) ->
             let la1 = l1 in
             Vec (la1 + l2)
                 (\ ii -> let i = ii in
                          if i < la1
                          then f1 i
                          else f2 i) ||]

for :: FO s => Qt (Int -> s -> (Int -> s -> s) -> s)
for = [|| \ l -> \ init -> \ step ->
              snd (while (\ t  -> fst t < l)
                         (\ tt -> let t  = tt    in
                                  let ft = fst t in
                                  (ft + 1
                                  , step ft (snd t)))
                         (0 , init))
          ||]

not :: Qt (Bool -> Bool)
not = [|| \ x -> if x then False else True ||]

and :: Qt (Bool -> Bool -> Bool)
and = [|| \ x -> \ y -> if x then y else False ||]

or :: Qt (Bool -> Bool -> Bool)
or = [|| \ x -> \ y -> if x then True else y ||]

notEql :: Eq t => Qt (t -> t -> Bool)
notEql = [|| \ x -> \ y -> $$not (x == y) ||]

gt :: (Eq t , Ord t) => Qt (t -> t -> Bool)
gt = [|| \ xx -> \ yy -> let x = xx in
                         let y = yy in
                         $$not ($$or (x < y) (x == y)) ||]

lte :: (Eq t , Ord t) => Qt (t -> t -> Bool)
lte = [|| \ xx -> \ yy -> let x = xx in
                          let y = yy in
                          $$or (x < y) (x == y) ||]

gte :: (Eq t , Ord t) => Qt (t -> t -> Bool)
gte = [|| \ x -> \ y -> $$not (x < y) ||]

min :: Ord t => Qt(t -> t -> t)
min = [|| \ xx -> \ yy -> let x = xx in
                          let y = yy in
                          if (x < y) then x else y ||]

pi :: Qt Float
pi =  let p = (P.negate P.pi) :: Float
      in  [|| p ||]

testBit    :: Qt (Int -> Int -> Bool)
testBit    = [|| \ i -> \ j -> if (i .&. (shfLft 1 j)) == 0
                                  then False
                                  else True ||]

oneBits :: Qt (Int -> Int)
oneBits    =  [|| \ n -> complement (shfLft (complement 0) n) ||]

lsbs :: Qt (Int -> Int -> Int)
lsbs       = [|| \ k -> \ i -> i .&. ($$oneBits k) ||]
