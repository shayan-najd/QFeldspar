module QFeldspar.Prelude.TemplateHaskell
       (Data,FO,toArr,fromArr,toArrF
       ,Int
       {-,true,false-}
       ,Cmx,cmx,real,imag
       ,Vec(..)
       ,Ary,mkArr,lnArr,ixArr
       ,forVec,frmTo,permute,reverse,foldl,fmap,zipWith,sum,scalarProd
       ,replicate,append
       {-(?)-},while,for,{-share-}memorize
       ,not,and,or
       ,Equality(eql),notEql
       ,Ordering(lt),gt,lte,gte,min
       ,Numeric(add,sub,mul,div),ilog2,pi
       ,bitXor,bitAnd,bitOr,shfRgt,shfLft,complement,testBit,lsbs
       ,oneBits,hashTable
       ,i2f,cis
       ,ret,bnd,may,mem
       ) where

import Prelude (toRational)

import QFeldspar.MyPrelude (Ary,Flt,Bol,Bool(True,False),Int
  ,Cmx,Maybe(..),fst,snd,Vec(..))
import qualified QFeldspar.MyPrelude as MP

import Language.Haskell.TH.Syntax (Lift(lift),Q,Exp(LitE),TExp
                                  ,Lit(IntegerL,RationalL))

import QFeldspar.Prelude.HaskellEnvironment

type Data t = Q (TExp t)

class    FO a                           where {}
instance FO Bol                         where {}
instance FO Int                         where {}
instance FO Flt                         where {}
instance (FO a , FO b) => FO (a , b)    where {}
instance FO a => FO (Ary a)             where {}
instance FO Cmx                         where {}

instance Lift Int where
  lift i = MP.return (LitE (IntegerL (MP.toInteger i)))

instance Lift Flt where
  lift f = MP.return (LitE (RationalL (toRational f)))

toArr        ::  Data (Vec a -> Ary a)
toArr        =   [|| \(Vec l g) -> mkArr l (\ i -> g i) ||]

fromArr      ::  Data (Ary a -> Vec a)
fromArr      =   [|| \ aa -> let a = aa in
                             Vec (lnArr a) (\i -> ixArr a i) ||]

toArrF ::  Data ((Vec a -> Vec b) -> Ary a -> Ary b)
toArrF =   [|| \ f a -> $$toArr (f ($$fromArr a)) ||]

-----------------------------------------------------------------------
-- Complex
-----------------------------------------------------------------------

cmx :: Flt -> Flt -> Cmx
cmx = MP.cmx

real :: Data (Cmx -> Flt)
real = [|| realPartHsk ||]

imag :: Data (Cmx -> Flt)
imag = [|| imagPartHsk ||]

-----------------------------------------------------------------------
-- Vector Operations
-----------------------------------------------------------------------
forVec :: FO s => Data (Int -> Vec s -> (Int -> Vec s -> Vec s) -> Vec s)
forVec = [|| \ l -> \ init -> \ step ->
             let init' = $$toArr init in
             let step' = \ i a -> $$toArr (step i ($$fromArr a)) in
             $$fromArr (snd (while
                             (\ t  -> $$lt (fst t) l)
                             (\ tt -> let t  = tt    in
                                      let ft = fst t in
                                      ( $$add ft 1
                                      , step' ft (snd t)))
                             (0 , init')))
          ||]

frmTo :: Data (Int -> Int -> Vec Int)
frmTo = [|| \ mm -> \ nn -> let n = nn in
                            let m = mm in
                            Vec
                            (if ($$lt n m)
                             then 0
                             else ($$add ($$sub n m) 1))
                            (\ i -> $$add i m) ||]

permute :: FO t => Data ((Int -> Int -> Int) -> Vec t -> Vec t)
permute = [|| \ f -> \ (Vec l g) -> let lv = l in
                            Vec lv (\ i -> g (f lv i)) ||]

reverse :: FO t => Data (Vec t -> Vec t)
reverse = [|| $$permute (\ l -> \ i -> $$sub ($$sub l 1) i) ||]

foldl :: (FO a , FO b) => Data ((a -> b -> a) -> a -> Vec b -> a)
foldl = [|| \ f -> \ acc -> \ (Vec l g) ->
            $$for l acc (\ i -> \ a -> f a (g i)) ||]

fmap :: (FO a , FO b) => Data ((a -> b) -> Vec a -> Vec b)
fmap = [|| \ f -> \ (Vec l g) -> Vec l (\ i -> f (g i)) ||]

zipWith :: (FO a , FO b , FO c) =>
           Data ((a -> b -> c) -> Vec a -> Vec b -> Vec c)
zipWith = [|| \ f -> \ (Vec l1 g1) -> \ (Vec l2 g2) ->
                Vec ($$min l1 l2)
                    (\ ii -> let i = ii in
                             f (g1 i) (g2 i)) ||]

sum :: Data (Vec Int -> Int)
sum = [|| $$foldl $$add 0 ||]

scalarProd :: Data (Vec Int -> Vec Int -> Int)
scalarProd  = [|| \ v1 -> \ v2 -> $$sum ($$zipWith $$mul v1 v2) ||]

replicate :: FO a => Data (Int -> a -> Vec a)
replicate = [|| \ n -> \ x -> Vec n (\ _i -> x) ||]

append :: FO a => Data (Vec a -> Vec a -> Vec a)
append = [|| \ (Vec l1 f1) -> \ (Vec l2 f2) ->
             let la1 = l1 in
             Vec ($$add la1 l2)
                 (\ ii -> let i = ii in
                          if $$lt i la1
                          then f1 i
                          else f2 i) ||]
{-
-----------------------------------------------------------------------
-- Array Operators
-----------------------------------------------------------------------

frmTo :: Data (Int -> Int -> Ary Int)
frmTo = [|| \ mm -> \ nn -> let n = nn in
                            let m = mm in
                            arr
                            (if ($$lt n m)
                             then 0
                             else ($$add ($$sub n m) 1))
                            (\ i -> $$add i m) ||]

permute :: FO t => Data ((Int -> Int -> Int) -> Ary t -> Ary t)
permute = [|| \ f -> \ v -> let lv = arrLen v in
                            arr lv (\ i -> ixArr v (f lv i)) ||]

reverse :: FO t => Data (Ary t -> Ary t)
reverse = [|| $$permute (\ l -> \ i -> $$sub ($$sub l 1) i) ||]

foldl :: (FO a , FO b) => Data ((a -> b -> a) -> a -> Ary b -> a)
foldl = [|| \ f -> \ acc -> \ v ->
            $$for (arrLen v) acc (\ i -> \ a -> f a (ixArr v i)) ||]

fmap :: (FO a , FO b) => Data ((a -> b) -> Ary a -> Ary b)
fmap = [|| \ f -> \ v -> arr (arrLen v) (\ i -> f (ixArr v i)) ||]

zipWith :: (FO a , FO b , FO c) =>
           Data ((a -> b -> c) -> Ary a -> Ary b -> Ary c)
zipWith = [|| \ f -> \ v1 -> \ v2 ->
                arr ($$min (arrLen v1) (arrLen v2))
                    (\ ii -> let i = ii in
                             f (ixArr v1 i) (ixArr v2 i)) ||]

sum :: Data (Ary Int -> Int)
sum = [|| $$foldl $$add 0 ||]

scalarProd :: Data (Ary Int -> Ary Int -> Int)
scalarProd  = [|| \ v1 -> \ v2 -> $$sum ($$zipWith $$mul v1 v2) ||]

replicate :: FO a => Data (Int -> a -> Ary a)
replicate = [|| \ n -> \ x -> arr n (\ _i -> x) ||]

append :: FO a => Data (Ary a -> Ary a -> Ary a)
append = [|| \ a1 -> \ a2 -> let la1 = arrLen a1 in
                             arr ($$add la1 (arrLen a2))
                                 (\ ii -> let i = ii in
                                          if $$lt i la1
                                          then ixArr a1 i
                                          else ixArr a2 i) ||]
-}
-----------------------------------------------------------------------
-- Control Flow
-----------------------------------------------------------------------

while :: FO s => (s -> Bol) -> (s -> s) -> s -> s
while = MP.while

for :: FO s => Data (Int -> s -> (Int -> s -> s) -> s)
for = [|| \ l -> \ init -> \ step ->
              snd (while (\ t  -> $$lt (fst t) l)
                         (\ tt -> let t  = tt    in
                                  let ft = fst t in
                                  ( $$add ft 1
                                  , step ft (snd t)))
                         (0 , init))
          ||]

memorize :: FO a => Data (a -> a)
memorize = [|| mem ||]

mem :: FO a => a -> a
mem = MP.mem

------------------------------------------------------------------------
-- Boolean Operators
------------------------------------------------------------------------

not :: Data (Bol -> Bol)
not = [|| \ x -> if x then False else True ||]

and :: Data (Bol -> Bol -> Bol)
and = [|| \ x -> \ y -> if x then y else False ||]

or :: Data (Bol -> Bol -> Bol)
or = [|| \ x -> \ y -> if x then True else y ||]

------------------------------------------------------------------------
-- Equality
------------------------------------------------------------------------

class Equality t where
  eql :: Data (t -> t -> Bol)

instance Equality Bol where
  eql = [|| eqlBolHsk ||]

instance Equality Int where
  eql = [|| eqlIntHsk ||]

instance Equality Flt where
  eql = [|| eqlFltHsk ||]

notEql :: Equality t => Data (t -> t -> Bol)
notEql= [|| \ x -> \ y -> $$not ($$eql x y) ||]

------------------------------------------------------------------------
-- Ordering
------------------------------------------------------------------------

class Ordering t where
  lt :: Data (t -> t -> Bol)

instance Ordering Bol where
  lt = [|| ltdBolHsk ||]

instance Ordering Int where
  lt = [|| ltdIntHsk ||]

instance Ordering Flt where
  lt = [|| ltdFltHsk ||]

gt :: (Equality t , Ordering t) => Data (t -> t -> Bol)
gt = [|| \ xx -> \ yy -> let x = xx in
                         let y = yy in
                         $$not ($$or ($$lt x y) ($$eql x y)) ||]

lte :: (Equality t , Ordering t) => Data (t -> t -> Bol)
lte = [|| \ xx -> \ yy -> let x = xx in
                          let y = yy in
                          $$or ($$lt x y) ($$eql x y) ||]

gte :: (Equality t , Ordering t) => Data (t -> t -> Bol)
gte = [|| \ x -> \ y -> $$not ($$lt x y) ||]

min :: Ordering t => Data(t -> t -> t)
min = [|| \ xx -> \ yy -> let x = xx in
                          let y = yy in
                          if ($$lt x y) then x else y ||]

------------------------------------------------------------------------
-- Numeric
------------------------------------------------------------------------

class Numeric t where
  add :: Data (t -> t -> t)
  sub :: Data (t -> t -> t)
  mul :: Data (t -> t -> t)
  div :: Data (t -> t -> t)

instance Numeric Int where
  add = [|| addIntHsk ||]
  sub = [|| subIntHsk ||]
  mul = [|| mulIntHsk ||]
  div = [|| divIntHsk ||]

instance Numeric Flt where
  add = [|| addFltHsk ||]
  sub = [|| subFltHsk ||]
  mul = [|| mulFltHsk ||]
  div = [|| divFltHsk ||]

instance Numeric (Cmx) where
  add = [|| addCmxHsk ||]
  sub = [|| subCmxHsk ||]
  mul = [|| mulCmxHsk ||]
  div = [|| divCmxHsk ||]

ilog2 :: Data (Int -> Int)
ilog2 = [|| ilog2Hsk ||]

pi :: Data Flt
pi =  let p = (MP.negate MP.pi) :: MP.Flt
      in  [|| p ||]

------------------------------------------------------------------------
-- Bitwise Operators
------------------------------------------------------------------------

bitXor :: Data (Int -> Int -> Int)
bitXor = [|| xorIntHsk ||]

bitAnd :: Data (Int -> Int -> Int)
bitAnd = [|| andIntHsk ||]

bitOr  :: Data (Int -> Int -> Int)
bitOr  = [|| orIntHsk ||]

shfRgt :: Data (Int -> Int -> Int)
shfRgt = [|| shrIntHsk ||]

shfLft :: Data (Int -> Int -> Int)
shfLft = [|| shlIntHsk ||]

complement :: Data (Int -> Int)
complement = [|| cmpIntHsk ||]

hashTable :: Data (Ary Int)
hashTable = [|| hshTblHsk ||]

testBit    :: Data (Int -> Int -> Bol)
testBit    = [|| \ i -> \ j -> if $$eql ($$bitAnd i ($$shfLft 1 j)) 0
                                  then False
                                  else True ||]

oneBits :: Data (Int -> Int)
oneBits    =  [|| \ n -> $$complement ($$shfLft ($$complement 0) n) ||]

lsbs :: Data (Int -> Int -> Int)
lsbs       = [|| \ k -> \ i -> $$bitAnd i ($$oneBits k) ||]

------------------------------------------------------------------------
-- Conversion Operators
------------------------------------------------------------------------

i2f :: Data (Int -> Flt)
i2f = [|| i2fHsk ||]

cis :: Data (Flt -> Cmx)
cis = [|| cisHsk ||]

------------------------------------------------------------------------
-- Option Type
------------------------------------------------------------------------

ret :: Data (a -> Maybe a)
ret = [|| \ x -> Just x ||]

bnd :: Data (Maybe a -> (a -> Maybe b) -> Maybe b)
bnd = [|| \ m -> \ k -> case m of {Nothing -> Nothing ; Just x -> k x} ||]

may :: Data (b -> (a -> b) -> Maybe a -> b)
may = [|| \ x -> \ g -> \ m -> case m of {Nothing -> x ; Just y  -> g y} ||]

------------------------------------------------------------------------
-- Ary
------------------------------------------------------------------------

mkArr :: Int -> (Int -> a) -> Ary a
mkArr = MP.mkArr

lnArr :: Ary a -> Int
lnArr = MP.lnArr

ixArr :: Ary a -> Int -> a
ixArr = MP.ixArr
