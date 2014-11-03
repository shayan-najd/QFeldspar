module QFeldspar.Prelude.MiniFeldspar
       (Data,Type
       ,Syn(..),toExpF,frmExpF
       ,Int
       ,Flt
       ,Bol,Fractional(..),true,false
       ,fst,snd
       ,Cmx,cmx,real,imag
       ,Vec,vec,len,ind,Ary
       ,frmTo,permute,reverse,foldl,map,zipWith,sum,scalarProd,replicate,append
       ,(?),ifThenElse,whl,forLoop,share,shared --memorize
       ,not,and,or
       ,Equality(eql),notEql
       ,Ordering(lt),gt,lte,gte,min
       ,add,sub,mul,div{-,neg-},ilog2,pi --sqrt
       ,bitXor,bitAnd,bitOr,shfRgt,shfLft,complement,testBit,lsbs,oneBits
       ,hashTable
       ,i2f,cis
       ,Undef(..)
       ,Opt,none,some,option
--     ,Ary, ary,lenA,indA
--     ,frmToA,permuteA,reverseA,foldlA,mapA,zipWithA,sumA,scalarProdA
--     ,replicateA,appendA
       ) where

import QFeldspar.MyPrelude (Int,Flt,Cmx,Ary,Bol,Num(..),Functor(..),Monad(..)
       ,fst,snd,Fractional(..),impossible,genNewNam,deepseq,($),(.))
import qualified QFeldspar.MyPrelude as MP

import QFeldspar.Expression.Feldspar.MiniFeldspar hiding (eql)
import qualified QFeldspar.Type.Feldspar.GADT    as TFG

import QFeldspar.Singleton
import QFeldspar.Environment.Typed (Env(Emp,Ext))
import QFeldspar.Prelude.Environment
import qualified QFeldspar.Variable.Typed as VT

type Type t = HasSin TFG.Typ t

type Data t = Exp Prelude t

prm0 :: (Type t, TFG.Arg t ~ '[]) =>
        VT.Var Prelude t -> Data (TFG.Out t)
prm0 v = AppV v Emp

prm1 :: (Type t, TFG.Arg t ~ '[t1]) =>
        VT.Var Prelude t -> Data t1 -> Data (TFG.Out t)
prm1 v e = AppV v (Ext e Emp)

prm2 :: (Type t, TFG.Arg t ~ '[t1, t2]) =>
        VT.Var Prelude t -> Data t1 -> Data t2 -> Data (TFG.Out t)
prm2 v e1 e2 = AppV v (Ext e1 (Ext e2 Emp))

---------------------------------------------------------------------------------
-- Syntactic Typeclass
---------------------------------------------------------------------------------

class Type (InT a) => Syn a where
  type InT a :: *
  toExp  :: a -> Data (InT a)
  frmExp :: Data (InT a) -> a

instance Type a => Syn (Data a) where
  type InT (Data a) = a
  toExp  x = x
  frmExp x = x

toExpF :: (Syn a , Syn b) => (a -> b) -> Data (InT a) -> Data (InT b)
toExpF f = toExp . f . frmExp

frmExpF :: (Syn a , Syn b) => (Data (InT a) -> Data (InT b)) -> a -> b
frmExpF f = frmExp . f . toExp

---------------------------------------------------------------------------------
-- Bool
---------------------------------------------------------------------------------

true :: Data Bol
true = ConB MP.True

false :: Data Bol
false = ConB MP.False

---------------------------------------------------------------------------------
-- Tuple
---------------------------------------------------------------------------------

instance (Syn a , Syn b) => Syn (a , b) where
    type InT (a , b) = (InT a , InT b)
    toExp (x , y) = Tpl (toExp x) (toExp y)
    frmExp ee     = let e = shared ee in
                    (frmExp (Fst e) , frmExp (Snd e))

---------------------------------------------------------------------------------
-- Complex
---------------------------------------------------------------------------------

cmx :: Data Flt -> Data Flt -> Data Cmx
cmx = Cmx

real :: Data Cmx -> Data Flt
real = prm1 realPartVar

imag :: Data Cmx -> Data Flt
imag = prm1 imagPartVar

---------------------------------------------------------------------------------
-- Vec
---------------------------------------------------------------------------------

data Vec t = Vec (Data Int) (Data Int -> t)

instance Syn a => Syn (Vec a) where
  type InT (Vec a) = Ary (InT a)
  toExp  v  = Ary (len v) (toExp . ind v)
  frmExp aa = let a = shared aa in
              vec (Len a) (\ i -> frmExp (Ind a i))

instance Functor Vec where
    fmap f (Vec n g) = Vec n (f . g)

vec :: Data Int -> (Data Int -> t) -> Vec t
vec = Vec

len :: Vec t -> Data Int
len (Vec l _) = l

ind :: Vec t -> Data Int -> t
ind (Vec _ f) = f

---------------------------------------------------------------------------------
-- Vector Operators
---------------------------------------------------------------------------------

frmTo :: Data Int -> Data Int -> Vec (Data Int)
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

foldl :: (Type (InT a) , Syn a , Syn b) =>
         (a -> b -> a) -> a -> Vec b -> a
foldl = \ f -> \ acc -> \ v ->
        forLoop (len v) acc (\ i -> \ a -> f a (ind v i))

map :: (a -> b) -> Vec a -> Vec b
map = fmap

zipWith :: (Syn a , Syn b , Syn c) =>
           (a -> b -> c) -> Vec a -> Vec b -> Vec c
zipWith = \ f -> \ v1 -> \ v2 ->
          vec (min (len v1) (len v2))
              (\ ii -> share ii (\ i ->
                       f (ind v1 i) (ind v2 i)))

sum :: (Syn a , Num a , Type (InT a)) => Vec a -> a
sum = foldl add 0

scalarProd :: Vec (Data Int) -> Vec (Data Int) -> Data Int
scalarProd = \ v1 -> \ v2 -> sum (zipWith mul v1 v2)

replicate :: Data Int -> a -> Vec a
replicate = \ n -> \ x -> vec n (\ _i -> x)

append :: Syn a => Vec a -> Vec a -> Vec a
append  = \ v1 -> \ v2 -> let lv1 = shared (len v1) in
                          vec (add lv1 (len v2))
                              (\ ii -> share ii (\ i ->
                                       (lt i lv1) ?
                                       (ind v1 i ,
                                        ind v2 i)))

---------------------------------------------------------------------------------
-- Control Flow
---------------------------------------------------------------------------------

share :: (Type (InT tl) , Syn tl , Syn tb) =>
         tl -> (tl -> tb) -> tb
share e f = frmExp (Let (toExp e) (toExp . f . frmExp))

{-# NOINLINE shared #-}
shared :: Exp r t -> Exp r t
shared e = let v = genNewNam "shared"
               {-# NOINLINE v #-}
           in  deepseq v $ Tag v e

(?) :: Syn a => Data Bol -> (a , a) -> a
c ? (t , e) = frmExp (Cnd c (toExp t) (toExp e))

ifThenElse :: Syn a => Data Bol -> a -> a -> a
ifThenElse c t e = c ? (t , e)

whl :: Syn a => (a -> Data Bol) -> (a -> a) -> a -> a
whl c b i = frmExp (Whl (c . frmExp) (toExp . b . frmExp) (toExp i))

forLoop :: (Type (InT a) , Syn a) =>
           Data Int -> a -> (Data Int -> a -> a) -> a
forLoop l init step = snd (whl (\ t -> lt (fst t) l)
                               (\ tt -> share tt (\ t ->
                                        share (fst t) (\ ft ->
                                        (add  ft 1 , step ft (snd t)))))
                               (0 , init))

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
  eql = prm2 eqlBolVar

instance Equality Int where
  eql = prm2 eqlIntVar

instance Equality Flt where
  eql = prm2 eqlFltVar

notEql :: Equality t => Data t -> Data t -> Data Bol
notEql = \ x -> \ y -> not (eql x y)

---------------------------------------------------------------------------------
-- Ordering
---------------------------------------------------------------------------------

class Ordering t where
  lt :: Data t -> Data t -> Data Bol

instance Ordering Bol where
  lt = prm2 ltdBolVar

instance Ordering Int where
  lt = prm2 ltdIntVar

instance Ordering Flt where
  lt = prm2 ltdFltVar

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
  (+) = prm2 addFltVar
  (-) = prm2 subFltVar
  (*) = prm2 mulFltVar
  fromInteger x = ConF (MP.fromInteger x)
  abs    = impossible
  signum = impossible

instance Num (Data Int) where
  (+) = prm2 addIntVar
  (-) = prm2 subIntVar
  (*) = prm2 mulIntVar
  fromInteger x = ConI (MP.fromInteger x)
  abs    = impossible
  signum = impossible

instance Num (Data Cmx) where
  (+) = prm2 addCmxVar
  (-) = prm2 subCmxVar
  (*) = prm2 mulCmxVar
  fromInteger x = Cmx (ConF (MP.fromInteger x)) 0.0
  abs    = impossible
  signum = impossible

instance Fractional (Data Flt) where
  (/) = prm2 divFltVar
  fromRational r = ConF (fromRational r)

add :: Num a => a -> a -> a
add = (+)

sub :: Num a => a -> a -> a
sub = (-)

mul :: Num a => a -> a -> a
mul = (*)

div :: Data Int -> Data Int -> Data Int
div = prm2 divIntVar

ilog2 :: Data Int -> Data Int
ilog2 = prm1 ilog2Var

pi :: Data Flt
pi = ConF (MP.negate MP.pi)

---------------------------------------------------------------------------------
-- Bitwise Operators
---------------------------------------------------------------------------------

bitXor :: Data Int -> Data Int -> Data Int
bitXor = prm2 xorIntVar

bitAnd :: Data Int -> Data Int -> Data Int
bitAnd = prm2 andIntVar

bitOr  :: Data Int -> Data Int -> Data Int
bitOr  = prm2 orIntVar

shfRgt :: Data Int -> Data Int -> Data Int
shfRgt = prm2 shrIntVar

shfLft :: Data Int -> Data Int -> Data Int
shfLft = prm2 shlIntVar

complement :: Data Int -> Data Int
complement = prm1 cmpIntVar

hashTable :: Data (Ary Int)
hashTable = prm0 hshTblVar

testBit    :: Data Int -> Data Int -> Data Bol
testBit    = \ i -> \ j -> ifThenElse (eql (bitAnd i (shfLft 1 j)) 0)
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
i2f = prm1 i2fVar

cis :: Data Flt -> Data Cmx
cis = prm1 cisVar

---------------------------------------------------------------------------------
-- Undefined
---------------------------------------------------------------------------------

class Syn a => Undef a where
  undef :: a

instance Undef (Data Bol) where
  undef = false

instance Undef (Data Int) where
  undef = 0

instance Undef (Data Flt) where
  undef = 0

instance (Undef a, Undef b) => Undef (a,b) where
  undef = (undef, undef)

---------------------------------------------------------------------------------
-- Option Type
---------------------------------------------------------------------------------

data Opt_R a = Opt_R { def :: Data Bol, val :: a }

instance Syn a => Syn (Opt_R a) where
  type InT (Opt_R a) =  (Bol, InT a)
  toExp (Opt_R b x)  =  Tpl b (toExp x)
  frmExp pp          =  let p = shared pp in
                        Opt_R (Fst p) (frmExp (Snd p))

some_R            ::  a -> Opt_R a
some_R x          =   Opt_R true x

none_R            ::  Undef a => Opt_R a
none_R            =   Opt_R false undef

option_R          ::  Syn b => b -> (a -> b) -> Opt_R a -> b
option_R d f o    =   def o ? (f (val o), d)

newtype Opt a = O { unO :: forall b . Undef b => ((a -> Opt_R b) -> Opt_R b) }

instance Monad Opt where
  return x    =  O (\g -> g x)
  m >>= k     =  O (\g -> unO m (\x -> unO (k x) g))

instance Undef a => Syn (Opt a) where
  type InT (Opt a) =  (Bol, InT a)
  frmExp           =  lift . frmExp
  toExp            =  toExp . lower

lift          ::  Opt_R a -> Opt a
lift o        =   O (\g -> Opt_R  (def o ? (def (g (val o)), false))
                                  (def o ? (val (g (val o)), undef)))

lower         ::  Undef a => Opt a -> Opt_R a
lower m       =   unO m some_R

some          ::  a -> Opt a
some a        =   lift (some_R a)

none          ::  Undef a => Opt a
none          =   lift none_R

option        ::  (Undef a, Undef b) => b -> (a -> b) -> Opt a -> b
option d f o  =   option_R d f (lower o)

{-
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

zipWithA :: (Type a , Type b , Type c) =>
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


-}
