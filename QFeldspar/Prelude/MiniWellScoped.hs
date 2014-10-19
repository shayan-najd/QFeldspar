module QFeldspar.Prelude.MiniWellScoped
       (Data
       ,Int
       ,Flt
       ,Bol,Bool(True,False),Fractional(..)
       ,Tpl,tpl,fst,snd
       ,Cmx,cmx,real,imag
       ,Vec,vec,lenV,indV
       ,Ary,ary,len,ind
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

import qualified Prelude   as P
import QFeldspar.MyPrelude (Int,Flt,Cmx,Ary,Bol,Tpl,Bool(..),Num(..)
                           ,Fractional(..),impossible)
import qualified QFeldspar.MyPrelude as MP

import QFeldspar.Expression.Feldspar.MiniWellScoped hiding (eql)
import qualified QFeldspar.Type.Feldspar.GADT            as TFG

import QFeldspar.Singleton
import QFeldspar.Environment.Typed (Env(Emp,Ext))
import QFeldspar.Prelude.Environment

import Data.Unique
import System.IO.Unsafe

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

tpl :: Data a -> Data b -> Data (Tpl a b)
tpl = Tpl

fst :: Type b => Data (Tpl a b) -> Data a
fst = Fst

snd :: Type a => Data (Tpl a b) -> Data b
snd = Snd

---------------------------------------------------------------------------------
-- Complex
---------------------------------------------------------------------------------

cmx :: Data Flt -> Data Flt -> Data Cmx
cmx = Cmx

real :: Data Cmx -> Data Flt
real e = AppV realPartVar (Ext e Emp)

imag :: Data Cmx -> Data Flt
imag e = AppV imagPartVar (Ext e Emp)

---------------------------------------------------------------------------------
-- Vec
---------------------------------------------------------------------------------

data Vec t = Vec (Data Int) (Data Int -> Data t)

vec :: Data Int -> (Data Int -> Data t) -> Vec t
vec = Vec

lenV :: Vec t -> Data Int
lenV (Vec l _) = l

indV :: Vec t -> Data Int -> Data t
indV (Vec _ f) = f

---------------------------------------------------------------------------------
-- Ary
---------------------------------------------------------------------------------

ary :: Data Int -> (Data Int -> Data t) -> Data (Ary t)
ary = Ary

len  :: Type t => Data (Ary t) -> Data Int
len = Len

ind :: Data (Ary t) -> Data Int -> Data t
ind = Ind

---------------------------------------------------------------------------------
-- Control Flow
---------------------------------------------------------------------------------

ifThenElse :: Data Bol -> Data a -> Data a -> Data a
ifThenElse = Cnd

whl :: (Data t -> Data Bol) -> (Data t -> Data t) -> Data t -> Data t
whl = Whl

share :: Type tl => Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb
share = Let

shared :: Exp r t -> Exp r t
shared e = Tag (unsafePerformIO (P.fmap (P.show P.. hashUnique) newUnique)) e

forLoop :: Type s => Data Int -> Data s ->
           (Data Int -> Data s -> Data s ) -> Data s
forLoop l init step = Snd (Whl (\ t -> lt (Fst t) l)
                               (\ t -> Tpl
                                       (add  (Fst t) (ConI 1))
                                       (step (Fst t) (Snd t)))
                               (Tpl (ConI 0) init))

forLoopVec :: Type s => Data Int -> Vec s ->
           (Data Int -> Vec s -> Vec s) -> Vec s
forLoopVec l init step =  let init'     = vec2ary init
                              step' i a = vec2ary (step i (ary2vec a))
                          in  ary2vec (forLoop l init' step')

---------------------------------------------------------------------------------
-- Boolean Operators
---------------------------------------------------------------------------------

not :: Data Bol -> Data Bol
not x = ifThenElse x false true

and :: Data Bol -> Data Bol -> Data Bol
and x y = ifThenElse x y false

or :: Data Bol -> Data Bol -> Data Bol
or x y = ifThenElse x true y

---------------------------------------------------------------------------------
-- Equality
---------------------------------------------------------------------------------

class Equality t where
  eql :: Data t -> Data t -> Data Bol

instance Equality Bol where
  eql el er = AppV eqlBolVar (Ext el (Ext er Emp))

instance Equality Int where
  eql el er = AppV eqlIntVar (Ext el (Ext er Emp))

instance Equality Flt where
  eql el er = AppV eqlFltVar (Ext el (Ext er Emp))

notEql :: Equality t => Data t -> Data t -> Data Bol
notEql x y = not (eql x y)

---------------------------------------------------------------------------------
-- Ordering
---------------------------------------------------------------------------------

class Ordering t where
  lt :: Data t -> Data t -> Data Bol

instance Ordering Bol where
  lt el er = AppV ltdBolVar (Ext el (Ext er Emp))

instance Ordering Int where
  lt el er = AppV ltdIntVar (Ext el (Ext er Emp))

instance Ordering Flt where
  lt el er = AppV ltdFltVar (Ext el (Ext er Emp))

gt :: (Equality t , Ordering t) => Data t -> Data t -> Data Bol
gt x y = not (or (lt x y) (eql x y))

lte :: (Equality t , Ordering t) => Data t -> Data t -> Data Bol
lte x y = or (lt x y) (eql x y)

gte :: (Equality t , Ordering t) => Data t -> Data t -> Data Bol
gte x y = not (lt x y)

min :: Ordering t => Data t -> Data t -> Data t
min x y = ifThenElse (lt x y) x y

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
  fromInteger x = Cmx (ConF (MP.fromInteger x)) (ConF 0.0)
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
  (ConI 31) - nlz xx
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

bitXor        :: Data Int -> Data Int -> Data Int
bitXor el er  = AppV xorIntVar (Ext el (Ext er Emp))

bitAnd      :: Data Int -> Data Int -> Data Int
bitAnd el er  = AppV andIntVar (Ext el (Ext er Emp))

bitOr      :: Data Int -> Data Int -> Data Int
bitOr  el er  = AppV orIntVar (Ext el (Ext er Emp))

shfRgt     :: Data Int -> Data Int -> Data Int
shfRgt el er  = AppV shrIntVar (Ext el (Ext er Emp))

shfLft     :: Data Int -> Data Int -> Data Int
shfLft el er  = AppV shlIntVar (Ext el (Ext er Emp))

complement :: Data Int -> Data Int
complement e  = AppV cmpIntVar (Ext e Emp)

testBit    :: Data Int -> Data Int -> Data Bol
testBit el er =  ifThenElse (eql (bitAnd el (shfLft (ConI 1) er)) (ConI 0))
                 false
                 true

oneBits :: Data Int -> Data Int
oneBits n = complement (shfLft (complement (ConI 0)) n)

lsbs :: Data Int -> Data Int -> Data Int
lsbs k i = bitAnd i (oneBits k)

---------------------------------------------------------------------------------
-- Conversion Operators
---------------------------------------------------------------------------------

i2f :: Data Int -> Data Flt
i2f e = AppV i2fVar (Ext e Emp)

cis :: Data Flt -> Data Cmx
cis e = AppV cisVar (Ext e Emp)

vec2ary :: Vec t -> Data (Ary t)
vec2ary v = Ary (lenV v) (indV v)

ary2vec :: Type t => Data (Ary t) -> Vec t
ary2vec v = let v' = shared v in vec (Len v') (\i -> Ind v' i)

---------------------------------------------------------------------------------
-- Vector Operators
---------------------------------------------------------------------------------

frmTo :: Data Int -> Data Int -> Vec Int
frmTo m n = vec
            (ifThenElse (lt n m)
             (ConI 0)
             (add (sub n m) (ConI 1)))
            (\ i -> add i m)

permute :: (Data Int -> Data Int -> Data Int)
           -> Vec t -> Vec t
permute f v = vec (lenV v) (\ i -> indV v (f (lenV v) i))

reverse :: Vec t -> Vec t
reverse = permute (\ l i -> sub (sub l (ConI 1)) i)

foldl :: Type a =>
         (Data a -> Data b -> Data a) -> Data a -> Vec b -> Data a
foldl f acc v  = forLoop (lenV v) acc (\ i a ->  f a (indV v i))


foldlVec :: Type a =>
         (Vec a -> Data b -> Vec a) -> Vec a ->
         Vec b -> Vec a
foldlVec f acc v  = let acc' = vec2ary acc
                        f' vv d = vec2ary (f (ary2vec vv) d)
                    in  ary2vec (forLoop (lenV v) acc' (\ i a ->  f' a (indV v i)))

map :: (Data a -> Data b) -> Vec a -> Vec b
map f v = vec (lenV v) (\i -> f (indV v i))

zipWith :: (Data a -> Data b -> Data c) -> Vec a -> Vec b -> Vec c
zipWith f v1 v2 = vec (min (lenV v1) (lenV v2))
                      (\ i -> f (indV v1 i) (indV v2 i))

sum :: Vec Int -> Data Int
sum = foldl add (ConI 0)

scalarProd :: Vec Int -> Vec Int -> Data Int
scalarProd v1 v2 = sum (zipWith mul v1 v2)

replicate :: Data Int -> Data a -> Vec a
replicate n x = vec n (\ _i -> x)

append :: Vec a -> Vec a -> Vec a
append v1 v2 = vec (add (lenV v1) (lenV v2))
                   (\ i -> ifThenElse (lt i (lenV v1))
                                      (indV v1 i)
                                      (indV v2 i))

---------------------------------------------------------------------------------
-- Ary Operators
---------------------------------------------------------------------------------

frmToA :: Data Int -> Data Int -> Data (Ary Int)
frmToA m n = Ary
             (ifThenElse (lt n m)
              (ConI 0)
              (add (sub n m) (ConI 1)))
             (\ i -> add i m)

permuteA :: Type t =>
            (Data Int -> Data Int -> Data Int)
           -> Data (Ary t) -> Data (Ary t)
permuteA f v = Ary (len v) (\ i -> ind v (f (len v) i))

reverseA :: Type t => Data (Ary t) -> Data (Ary t)
reverseA = permuteA (\ l i -> sub (sub l (ConI 1)) i)

foldlA :: (Type a , Type b) =>
         (Data a -> Data b -> Data a) -> Data a -> Data (Ary b) -> Data a
foldlA f acc v  = forLoop (len v) acc (\ i a -> f a (ind v i))

mapA :: Type a =>
        (Data a -> Data b) -> Data (Ary a) -> Data (Ary b)
mapA f v = Ary (len v) (\ i -> f (ind v i))

zipWithA :: (Type a , Type b) =>
            (Data a -> Data b -> Data c) -> Data (Ary a) -> Data (Ary b) ->
           Data (Ary c)
zipWithA f v1 v2 = Ary (min (len v1) (len v2))
                      (\ i -> f (ind v1 i) (ind v2 i))

sumA :: Data (Ary Int) -> Data Int
sumA = foldlA add (ConI 0)

scalarProdA :: Data (Ary Int) -> Data (Ary Int) -> Data Int
scalarProdA v1 v2 = sumA (zipWithA mul v1 v2)

replicateA :: Data Int -> Data a -> Data (Ary a)
replicateA n x = ary n (\ _i -> x)

appendA ::Type a => Data (Ary a) -> Data (Ary a) -> Data (Ary a)
appendA a1 a2 = ary (add (len a1) (len a2))
                    (\ i -> ifThenElse (lt i (len a1))
                                       (ind a1 i)
                                       (ind a2 i))

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
