module Examples.Prelude.QDSL where

import qualified Prelude as P
import QFeldspar.QDSL

toArr        ::  Qt (Vec a -> Ary a)
toArr        =   [|| \(Vec l g) -> mkArr l (\ i -> g i) ||]

fromArr      ::  Qt (Ary a -> Vec a)
fromArr      =   [|| \ aa -> let a = aa in
                             Vec (lnArr a) (\i -> ixArr a i) ||]

toArrF ::  Qt ((Vec a -> Vec b) -> Ary a -> Ary b)
toArrF =   [|| \ f a -> $$toArr (f ($$fromArr a)) ||]

frmTo :: Qt (Word32 -> Word32 -> Vec Word32)
frmTo = [|| \ mm -> \ nn -> let n = nn in
                            let m = mm in
                            Vec
                            (if n < m
                             then 0
                             else n - m + 1)
                            (\ i -> i + m) ||]

permute :: FO t => Qt ((Word32 -> Word32 -> Word32) -> Vec t -> Vec t)
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

sum :: Qt (Vec Word32 -> Word32)
sum = [|| $$foldl (+) 0 ||]

scalarProd :: Qt (Vec Word32 -> Vec Word32 -> Word32)
scalarProd  = [|| \ v1 -> \ v2 -> $$sum ($$zipWith (*) v1 v2) ||]

fmap :: (FO a , FO b) => Qt ((a -> b) -> Vec a -> Vec b)
fmap = [|| \ f -> \ (Vec l g) -> Vec l (\ i -> f (g i)) ||]

forVec :: FO s =>
          Qt (Word32 -> Vec s -> (Word32 -> Vec s -> Vec s) -> Vec s)
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

replicate :: FO a => Qt (Word32 -> a -> Vec a)
replicate = [|| \ n -> \ x -> Vec n (\ _i -> x) ||]

append :: FO a => Qt (Vec a -> Vec a -> Vec a)
append = [|| \ (Vec l1 f1) -> \ (Vec l2 f2) ->
             let la1 = l1 in
             Vec (la1 + l2)
                 (\ ii -> let i = ii in
                          if i < la1
                          then f1 i
                          else f2 i) ||]

for :: FO s => Qt (Word32 -> s -> (Word32 -> s -> s) -> s)
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

testBit    :: Qt (Word32 -> Word32 -> Bool)
testBit    = [|| \ i -> \ j -> if (i .&. (shfLft 1 j)) == 0
                                  then False
                                  else True ||]

oneBits :: Qt (Word32 -> Word32)
oneBits    =  [|| \ n -> complement (shfLft (complement 0) n) ||]

lsbs :: Qt (Word32 -> Word32 -> Word32)
lsbs       = [|| \ k -> \ i -> i .&. ($$oneBits k) ||]

lenV :: Qt (Vec a -> Word32)
lenV = [|| \ (Vec l _ixf) -> l ||]

indV :: Qt (Vec a -> Word32 -> a)
indV = [|| \ (Vec _l ixf) -> ixf ||]

type Pixel = (Word32,(Word32,Word32))

mkPixel :: Qt (Word32 -> Word32 -> Word32 -> Pixel)
mkPixel = [|| \ r g b -> (r , (g , b)) ||]

red :: Qt (Pixel -> Word32)
red = [|| \ pixel -> fst pixel ||]

green :: Qt (Pixel -> Word32)
green = [|| \ pixel -> fst (snd pixel) ||]

blue :: Qt (Pixel -> Word32)
blue = [|| \ pixel -> snd (snd pixel) ||]

-- non-empty image
type Image = Vec (Vec Pixel)

mkImage :: Qt (Word32 -> Word32 ->
                (Word32 -> Word32 -> Pixel) -> Image)
mkImage = [|| \ height width ixf ->
  Vec height (\ i ->
    Vec width (\ j -> ixf i j)) ||]

heightImage :: Qt (Image -> Word32)
heightImage = [|| \ image -> $$lenV image ||]

widthImage :: Qt (Image -> Word32)
widthImage = [|| \ image -> $$lenV ($$indV image 0) ||]

getPixel :: Qt (Image -> Word32 -> Word32 -> Pixel)
getPixel = [|| \ vec i j -> $$indV ($$indV vec i) j ||]

aryToImage :: Qt (Word32 -> Word32 -> Ary Word32 -> Image)
aryToImage = [|| \ height width as ->
                 $$mkImage height width (\ i j ->
                     $$mkPixel (ixArr as ((j+i*width)*3))
                               (ixArr as ((j+i*width)*3+1))
                               (ixArr as ((j+i*width)*3+2))) ||]

imageToAry :: Qt (Image -> Ary Word32)
imageToAry = [|| \ image -> let height = $$heightImage image
                                width  = $$widthImage  image
                            in  mkArr (height * width * 3)
                                (\ ii -> let i = div (div ii 3) width
                                             j = mod (div ii 3) width
                                             p = $$getPixel image i j
                                         in if mod ii 3 == 0
                                            then $$red   p
                                            else if mod ii 3 == 1
                                            then $$green p
                                            else $$blue  p) ||]
