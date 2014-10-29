module QFeldspar.Prelude.MiniWellScoped
       (Data
       ,Int
       ,Flt
       ,Bol,Bool(True,False),Fractional(..)
       ,Tpl,tpl,fst,snd
       ,Cmx,cmx,real,imag
       ,Vec,vec,len,ind
       ,Ary,ary,lenA,indA
       ,ifThenElse,whl,forLoop,forLoopVec,share,shared --memorize
       ,not,and,or
       ,Equality(eql),notEql
       ,Ordering(lt),gt,lte,gte,min
       ,add,sub,mul,div{-,neg-},ilog2,pi --sqrt
       ,bitXor,bitAnd,bitOr,shfRgt,shfLft,complement,testBit,lsbs,oneBits
       ,i2f,cis,ary2vec,vec2ary
       ,frmTo,permute,reverse,foldl,foldlVec,map,zipWith,sum,scalarProd,fromList
       ,replicate,append
       ,frmToA,permuteA,reverseA,foldlA,mapA,zipWithA,sumA,scalarProdA,fromListA
       ,replicateA,appendA,hashTable
       ) where

import QFeldspar.MyPrelude (Int,Flt,Cmx,Ary,Bol,Tpl,Bool(..),Num(..)
                           ,Fractional(..),impossible,genNewNam,deepseq,($))
import qualified QFeldspar.MyPrelude as MP

import QFeldspar.Expression.Feldspar.MiniWellScoped hiding (eql)
import qualified QFeldspar.Type.Feldspar.GADT            as TFG

import QFeldspar.Singleton
import QFeldspar.Environment.Typed (Env(Emp,Ext))
import QFeldspar.Prelude.Environment

type Type t = HasSin TFG.Typ t

type Data t = Exp Prelude t

---------------------------------------------------------------------------------
-- Bool
---------------------------------------------------------------------------------
true :: Exp r Bol
true = ConB MP.True

false :: Exp r Bol
false = ConB MP.False

---------------------------------------------------------------------------------
-- Tuple
---------------------------------------------------------------------------------

tpl :: Data a -> Data b -> Data (a , b)
tpl = Tpl

fst :: Type b => Data (a , b) -> Data a
fst = Fst

snd :: Type a => Data (a , b) -> Data b
snd = Snd

---------------------------------------------------------------------------------
-- Complex
---------------------------------------------------------------------------------

cmx :: Data Flt -> Data Flt -> Data Cmx
cmx = Cmx

real :: Data Cmx -> Data Flt
real = \ e -> AppV realPartVar (Ext e Emp)

imag :: Data Cmx -> Data Flt
imag = \ e -> AppV imagPartVar (Ext e Emp)

---------------------------------------------------------------------------------
-- Vec
---------------------------------------------------------------------------------

data Vec t = Vec (Data Int) (Data Int -> Data t)

vec :: Data Int -> (Data Int -> Data t) -> Vec t
vec = Vec

len :: Vec t -> Data Int
len (Vec l _) = l

ind :: Vec t -> Data Int -> Data t
ind (Vec _ f) = f

---------------------------------------------------------------------------------
-- Ary
---------------------------------------------------------------------------------

ary :: Data Int -> (Data Int -> Data t) -> Data (Ary t)
ary = Ary

lenA  :: Type t => Data (Ary t) -> Data Int
lenA = Len

indA :: Data (Ary t) -> Data Int -> Data t
indA = Ind

---------------------------------------------------------------------------------
-- Control Flow
---------------------------------------------------------------------------------

share :: Type tl => Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb
share = Let

{-# NOINLINE shared #-}
shared :: Exp r t -> Exp r t
shared e = let v = genNewNam "shared"
               {-# NOINLINE v #-}
           in deepseq v $ Tag v e

ifThenElse :: Data Bol -> Data a -> Data a -> Data a
ifThenElse = Cnd

whl :: (Data t -> Data Bol) -> (Data t -> Data t) -> Data t -> Data t
whl = Whl

forLoop :: Type s => Data Int -> Data s ->
           (Data Int -> Data s -> Data s ) -> Data s
forLoop l init step = snd (Whl (\ t -> lt (fst t) l)
                               (\ tt -> share tt (\ t ->
                                        share (fst t) (\ ft ->
                                        Tpl
                                        (add  ft 1)
                                        (step ft (snd t)))))
                               (Tpl 0 init))

forLoopVec :: Type s => Data Int -> Vec s ->
           (Data Int -> Vec s -> Vec s) -> Vec s
forLoopVec l init step = ary2vec (forLoop l (vec2ary init)
                         (\ i -> \ a -> vec2ary (step i (ary2vec a))))

---------------------------------------------------------------------------------
-- Boolean Operators
---------------------------------------------------------------------------------

not :: Data Bol -> Data Bol
not = \ x -> ifThenElse x false true

and :: Data Bol -> Data Bol -> Data Bol
and = \ x -> \ y -> ifThenElse x y false

or :: Data Bol -> Data Bol -> Data Bol
or = \ x -> \ y -> ifThenElse x true y

---------------------------------------------------------------------------------
-- Equality
---------------------------------------------------------------------------------

class Equality t where
  eql :: Data t -> Data t -> Data Bol

instance Equality Bol where
  eql = \ el -> \ er -> AppV eqlBolVar (Ext el (Ext er Emp))

instance Equality Int where
  eql = \ el -> \ er -> AppV eqlIntVar (Ext el (Ext er Emp))

instance Equality Flt where
  eql = \ el -> \ er -> AppV eqlFltVar (Ext el (Ext er Emp))

notEql :: Equality t => Data t -> Data t -> Data Bol
notEql x y = not (eql x y)

---------------------------------------------------------------------------------
-- Ordering
---------------------------------------------------------------------------------

class Ordering t where
  lt :: Data t -> Data t -> Data Bol

instance Ordering Bol where
  lt = \ el -> \ er -> AppV ltdBolVar (Ext el (Ext er Emp))

instance Ordering Int where
  lt = \ el -> \ er -> AppV ltdIntVar (Ext el (Ext er Emp))

instance Ordering Flt where
  lt = \ el -> \ er -> AppV ltdFltVar (Ext el (Ext er Emp))

gt :: (Equality t , Ordering t , Type t) => Data t -> Data t -> Data Bol
gt =  \ xx -> \ yy -> share xx (\ x ->
                      share yy (\ y ->
                      not (or (lt x y) (eql x y))))

lte :: (Equality t , Ordering t , Type t) => Data t -> Data t -> Data Bol
lte = \ xx -> \ yy -> share xx (\ x ->
                      share yy (\ y ->
                      or (lt x y) (eql x y)))

gte :: (Equality t , Ordering t) => Data t -> Data t -> Data Bol
gte = \ x -> \ y -> not (lt x y)

min :: (Ordering t , Type t) => Data t -> Data t -> Data t
min xx yy = share xx (\ x ->
            share yy (\ y ->
            ifThenElse (lt x y) x y))

---------------------------------------------------------------------------------
-- Numeric
---------------------------------------------------------------------------------
instance Num (Data Flt) where
  el + er = AppV addFltVar (Ext el (Ext er Emp))
  el - er = AppV subFltVar (Ext el (Ext er Emp))
  el * er = AppV mulFltVar (Ext el (Ext er Emp))
  fromInteger x = ConF (MP.fromInteger x)
  abs = impossible
  signum = impossible

instance Num (Data Int) where
  el + er = AppV addIntVar (Ext el (Ext er Emp))
  el - er = AppV subIntVar (Ext el (Ext er Emp))
  el * er = AppV mulIntVar (Ext el (Ext er Emp))
  fromInteger x = ConI (MP.fromInteger x)
  abs    = impossible
  signum = impossible

instance Num (Data Cmx) where
  el + er = AppV addCmxVar (Ext el (Ext er Emp))
  el - er = AppV subCmxVar (Ext el (Ext er Emp))
  el * er = AppV mulCmxVar (Ext el (Ext er Emp))
  fromInteger x = Cmx (ConF (MP.fromInteger x)) 0.0
  abs    = impossible
  signum = impossible

instance Fractional (Data Flt) where
    el / er = AppV divFltVar (Ext el (Ext er Emp))
    fromRational r = ConF (fromRational r)

add :: Num a => a -> a -> a
add = (+)

sub :: Num a => a -> a -> a
sub = (-)

mul :: Num a => a -> a -> a
mul = (*)

div :: Data Int -> Data Int -> Data Int
div el er = AppV divIntVar (Ext el (Ext er Emp))

ilog2 :: Data Int -> Data Int
ilog2 xx = AppV ilog2Var (Ext xx Emp)
  {-
  31 - nlz xx
 where
   nlz :: Data Int -> Data Int
   nlz x = bitCount (complement (part x))

   part :: Data Int -> Data Int
   part x = MP.foldl go x [1,2,4,8,16]
       where
         go :: Data Int -> MP.Int -> Data Int
         go b s = b .|. (b .>>. (litI s))
  -}

pi :: Data Flt
pi = ConF (MP.negate MP.pi)

---------------------------------------------------------------------------------
-- Bitwise Operators
---------------------------------------------------------------------------------

bitXor :: Data Int -> Data Int -> Data Int
bitXor = \ el -> \ er -> AppV xorIntVar (Ext el (Ext er Emp))

bitAnd :: Data Int -> Data Int -> Data Int
bitAnd = \ el -> \ er -> AppV andIntVar (Ext el (Ext er Emp))

bitOr  :: Data Int -> Data Int -> Data Int
bitOr  = \ el -> \ er -> AppV orIntVar (Ext el (Ext er Emp))

shfRgt :: Data Int -> Data Int -> Data Int
shfRgt = \ el -> \ er -> AppV shrIntVar (Ext el (Ext er Emp))

shfLft :: Data Int -> Data Int -> Data Int
shfLft = \ el -> \ er -> AppV shlIntVar (Ext el (Ext er Emp))

complement :: Data Int -> Data Int
complement =  \ e -> AppV cmpIntVar (Ext e Emp)

testBit    :: Data Int -> Data Int -> Data Bol
testBit    =  \ i -> \ j -> ifThenElse (eql (bitAnd i (shfLft 1 j)) 0)
                            false
                            true

oneBits :: Data Int -> Data Int
oneBits    = \ n -> complement (shfLft (complement 0) n)

lsbs :: Data Int -> Data Int -> Data Int
lsbs       = \ k -> \ i -> bitAnd i (oneBits k)

---------------------------------------------------------------------------------
-- Conversion Operators
---------------------------------------------------------------------------------

i2f :: Data Int -> Data Flt
i2f = \ e -> AppV i2fVar (Ext e Emp)

cis :: Data Flt -> Data Cmx
cis = \ e -> AppV cisVar (Ext e Emp)

vec2ary :: Vec t -> Data (Ary t)
vec2ary v = ary (len v) (ind v)

ary2vec :: Type t => Data (Ary t) -> Vec t
ary2vec v = let v' = shared v in vec (Len v') (\i -> Ind v' i)

---------------------------------------------------------------------------------
-- Vector Operators
---------------------------------------------------------------------------------

frmTo :: Data Int -> Data Int -> Vec Int
frmTo = \ mm -> \ nn -> let n = shared nn in
                        let m = shared mm in
                        vec
                        (ifThenElse (lt n m)
                         0
                         (add (sub n m) 1))
                        (\ i -> add i m)

permute :: (Data Int -> Data Int -> Data Int)
           -> Vec t -> Vec t
permute = \ f -> \ v -> let lv = shared (len v) in
                        vec lv (\ i -> ind v (f lv i))

reverse :: Vec t -> Vec t
reverse = permute (\ l -> \ i -> sub (sub l 1) i)

foldl :: Type a =>
         (Data a -> Data b -> Data a) -> Data a -> Vec b -> Data a
foldl = \ f -> \ acc -> \ v ->
        forLoop (len v) acc (\ i a -> f a (ind v i))

foldlVec :: Type a =>
         (Vec a -> Data b -> Vec a) -> Vec a ->
         Vec b -> Vec a
foldlVec f acc v  = ary2vec (forLoop (len v) (vec2ary acc)
                             (\ i a -> vec2ary (f (ary2vec a) (ind v i))))

map :: (Data a -> Data b) -> Vec a -> Vec b
map = \ f -> \ v -> vec (len v) (\ i -> f (ind v i))

zipWith :: (Data a -> Data b -> Data c) -> Vec a -> Vec b -> Vec c
zipWith = \ f -> \ v1 -> \ v2 ->
          vec (min (len v1) (len v2))
              (\ ii -> share ii (\ i ->
                       f (ind v1 i) (ind v2 i)))

sum :: Vec Int -> Data Int
sum = foldl add 0

scalarProd :: Vec Int -> Vec Int -> Data Int
scalarProd = \ v1 -> \ v2 -> sum (zipWith mul v1 v2)

replicate :: Data Int -> Data a -> Vec a
replicate = \ n -> \ x -> vec n (\ _i -> x)

append :: Vec a -> Vec a -> Vec a
append  = \ v1 -> \ v2 -> let lv1 = shared (len v1) in
                          vec (add lv1 (len v2))
                              (\ ii -> share ii (\ i ->
                                       ifThenElse (lt i lv1)
                                                  (ind v1 i)
                                                  (ind v2 i)))

---------------------------------------------------------------------------------
-- Ary Operators
---------------------------------------------------------------------------------

frmToA :: Data Int -> Data Int -> Data (Ary Int)
frmToA = \ mm -> \ nn -> let n = shared nn in
                         let m = shared mm in
                         ary
                         (ifThenElse (lt n m)
                                         0
                                         (add (sub n m) 1))
                         (\ i -> add i m)

permuteA :: Type t =>
            (Data Int -> Data Int -> Data Int)
           -> Data (Ary t) -> Data (Ary t)
permuteA = \ f -> \ v -> let lv = shared (lenA v) in
                         ary lv (\ i -> indA v (f lv i))


reverseA :: Type t => Data (Ary t) -> Data (Ary t)
reverseA = permuteA (\ l -> \ i -> sub (sub l 1) i)

foldlA :: (Type a , Type b) =>
         (Data a -> Data b -> Data a) -> Data a -> Data (Ary b) -> Data a
foldlA  = \ f -> \ acc -> \ v ->
           forLoop (lenA v) acc (\ i a -> f a (indA v i))

mapA :: Type a =>
        (Data a -> Data b) -> Data (Ary a) -> Data (Ary b)
mapA = \ f -> \ v -> ary (lenA v) (\ i -> f (indA v i))

zipWithA :: (Type a , Type b) =>
            (Data a -> Data b -> Data c) -> Data (Ary a) -> Data (Ary b) ->
           Data (Ary c)
zipWithA = \ f -> \ v1 -> \ v2 ->
           ary (min (lenA v1) (lenA v2))
                   (\ ii -> share ii (\ i ->
                            f (indA v1 i) (indA v2 i)))

sumA :: Data (Ary Int) -> Data Int
sumA = foldlA add 0

scalarProdA :: Data (Ary Int) -> Data (Ary Int) -> Data Int
scalarProdA  = \ v1 -> \ v2 -> sumA (zipWithA mul v1 v2)

replicateA :: Data Int -> Data a -> Data (Ary a)
replicateA n x = ary n (\ _i -> x)

appendA ::Type a => Data (Ary a) -> Data (Ary a) -> Data (Ary a)
appendA  = \ a1 -> \ a2 -> let lv1 = shared (lenA a1) in
           ary (add lv1 (lenA a2))
               (\ ii -> share ii (\i ->
                        ifThenElse (lt i lv1)
                                   (indA a1 i)
                                   (indA a2 i)))

---------------------------------------------------------------------------------
-- Helper Operators
---------------------------------------------------------------------------------

fromList:: [Data a] -> Data a -> Vec a
fromList lst k =  vec
                  (ConI (MP.fromIntegral (MP.length lst)))
                  (\ i ->  MP.foldr
                           (\ j acc -> ifThenElse
                                       (eql i
                                        (ConI (MP.fromIntegral j)))
                                       (lst MP.!! j)
                                       acc)
                           k
                           (MP.enumFromTo 0 (MP.length lst MP.- 1)))

fromListA :: [Data a] -> Data a -> Data (Ary a)
fromListA lst k = ary
                  (ConI (MP.fromIntegral (MP.length lst)))
                  (\ i ->  MP.foldr
                           (\ j acc -> ifThenElse
                                       (eql i
                                        (ConI (MP.fromIntegral j)))
                                       (lst MP.!! j)
                                       acc)
                           k
                           (MP.enumFromTo 0 (MP.length lst MP.- 1)))

hashTable :: Data (Ary Int)
hashTable = AppV hshTblVar Emp
