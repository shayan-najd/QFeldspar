module QFeldspar.Prelude.MiniFeldspar
       (Data,Type,Syn(..),toExpF,frmExpF
       ,Int
       ,true,false
       ,Cmx,cmx,real,imag
       ,Vec(..)
       ,frmTo,permute,reverse,foldl,zipWith,sum,scalarProd
       ,replicate,append
       ,Ary,arr,arrLen,arrIx
--     ,frmToA,permuteA,reverseA,foldlA,mapA,zipWithA,sumA,scalarProdA
--     ,replicateA,appendA
       ,(?),while,for,share,shared --memorize
       ,not,and,or
       ,Equality(eql),notEql
       ,Ordering(lt),gt,lte,gte,min
       ,add,sub,mul,div,ilog2,pi
       ,bitXor,bitAnd,bitOr,shfRgt,shfLft,complement,testBit,lsbs
       ,oneBits,hashTable
       ,i2f,cis
       ,Undef(..)
       ,Opt,none,some,option
       ,trmEql,trmEqlF
       ) where

import QFeldspar.MyPrelude (Int,Flt,Cmx,Ary,Bol,Num(..)
  ,Functor(..),Monad(..),fst,snd,Fractional(..),impossible -- ,genNewNam
  {-,deepseq,($) -},(.))
import qualified QFeldspar.MyPrelude as MP

import QFeldspar.Expression.MiniFeldspar
import qualified QFeldspar.Expression.Utils.MiniFeldspar as FMWS (eql,eqlF)
import qualified QFeldspar.Type.GADT    as TFG

import QFeldspar.Singleton
import QFeldspar.Environment.Typed (Env(Emp,Ext))
import QFeldspar.Prelude.Environment
import QFeldspar.Prelude.HaskellEnvironment
import qualified QFeldspar.Variable.Typed as VT
import QFeldspar.Expression.Utils.MiniFeldspar (shared)

type Data t = Exp Prelude t

type Type t = HasSin TFG.Typ t

prm0 :: (Type t, TFG.Arg t ~ '[]) =>
        VT.Var Prelude t -> Data (TFG.Out t)
prm0 v = AppV v Emp

prm1 :: (Type t, TFG.Arg t ~ '[t1]) =>
        VT.Var Prelude t -> Data t1 -> Data (TFG.Out t)
prm1 v e = AppV v (Ext e Emp)

prm2 :: (Type t, TFG.Arg t ~ '[t1, t2]) =>
        VT.Var Prelude t -> Data t1 -> Data t2 -> Data (TFG.Out t)
prm2 v e1 e2 = AppV v (Ext e1 (Ext e2 Emp))

trmEql :: Data t -> Data t -> MP.Bool
trmEql  = FMWS.eql

trmEqlF :: (Data a -> Data b) -> (Data a -> Data b) -> MP.Bool
trmEqlF = FMWS.eqlF
-----------------------------------------------------------------------
-- Syntactic Typeclass
-----------------------------------------------------------------------

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

-----------------------------------------------------------------------
-- Bool
-----------------------------------------------------------------------

true :: Data Bol
true = ConB MP.True

false :: Data Bol
false = ConB MP.False

-----------------------------------------------------------------------
-- Tuple
-----------------------------------------------------------------------

instance (Syn a , Syn b) => Syn (a , b) where
    type InT (a , b) = (InT a , InT b)
    toExp (x , y)    = Tpl (toExp x) (toExp y)
    frmExp ee        = let e = $shared ee in
                       (frmExp (Fst e) , frmExp (Snd e))

-----------------------------------------------------------------------
-- Complex
-----------------------------------------------------------------------

cmx :: Data Flt -> Data Flt -> Data Cmx
cmx = Cmx

real :: Data Cmx -> Data Flt
real = prm1 realPartVar

imag :: Data Cmx -> Data Flt
imag = prm1 imagPartVar

-----------------------------------------------------------------------
-- Vec
-----------------------------------------------------------------------

data Vec t = Vec (Data Int) (Data Int -> t)

instance Syn a => Syn (Vec a) where
  type InT (Vec a)  = Ary (InT a)
  toExp  (Vec l f)  = Ary l (\ i -> toExp (f i))
  frmExp aa         = let a = $shared aa in
                      Vec (Len a) (\ i -> frmExp (Ind a i))

instance Functor Vec where
    fmap f (Vec n g) = Vec n (f . g)

-----------------------------------------------------------------------
-- Vector Operators
-----------------------------------------------------------------------

frmTo :: Data Int -> Data Int -> Vec (Data Int)
frmTo = \ mm -> \ nn -> let n = $shared nn in
                        let m = $shared mm in
                        Vec
                        ((lt n m) ?
                         (0 ,
                         add (sub n m) 1))
                        (\ i -> add i m)

permute :: (Data Int -> Data Int -> Data Int)
           -> Vec t -> Vec t
permute = \ f -> \ (Vec l g) -> let lv = $shared l in
                                Vec lv (\ i -> g (f lv i))

reverse :: Vec t -> Vec t
reverse = permute (\ l -> \ i -> sub (sub l 1) i)

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
sum = foldl add 0

scalarProd :: Vec (Data Int) -> Vec (Data Int) -> Data Int
scalarProd = \ v1 -> \ v2 -> sum (zipWith mul v1 v2)

replicate :: Data Int -> a -> Vec a
replicate = \ n -> \ x -> Vec n (\ _i -> x)

append :: Syn a => Vec a -> Vec a -> Vec a
append  = \ (Vec l1 f1) -> \ (Vec l2 f2) ->
          let lv1 = $shared l1 in
          Vec (add lv1 l2)
              (\ ii -> share ii (\ i ->
                       (lt i lv1) ?
                         (f1 i ,
                          f2 i)))

-----------------------------------------------------------------------
-- Control Flow
-----------------------------------------------------------------------

share :: (Type (InT tl) , Syn tl , Syn tb) =>
         tl -> (tl -> tb) -> tb
share e f = frmExp (Let (toExp e) (toExp . f . frmExp))

{-
{-# NOINLINE shared #-}
shared :: Exp r t -> Exp r t
shared e = let v = genNewNam "shared"
               {-# NOINLINE v #-}
           in  deepseq v $ Tag v e
-}

(?) :: Syn a => Data Bol -> (a , a) -> a
c ? (t , e) = frmExp (Cnd c (toExp t) (toExp e))

while :: Syn a => (a -> Data Bol) -> (a -> a) -> a -> a
while c b i = frmExp (Whl (toExpF c) (toExpF b) (toExp i))

for :: (Type (InT a) , Syn a) =>
           Data Int -> a -> (Data Int -> a -> a) -> a
for l init step = snd (while (\ t -> lt (fst t) l)
                             (\ tt -> share tt (\ t ->
                                      share (fst t) (\ ft ->
                                      (add  ft 1 , step ft (snd t)))))
                               (0 , init))

-----------------------------------------------------------------------
-- Boolean Operators
-----------------------------------------------------------------------

not :: Data Bol -> Data Bol
not = \ x -> x ? (false , true)

and :: Data Bol -> Data Bol -> Data Bol
and = \ x -> \ y -> x ? (y , false)

or :: Data Bol -> Data Bol -> Data Bol
or = \ x -> \ y -> x ? (true , y)

-----------------------------------------------------------------------
-- Equality
-----------------------------------------------------------------------

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

-----------------------------------------------------------------------
-- Ordering
-----------------------------------------------------------------------

class Ordering t where
  lt :: Data t -> Data t -> Data Bol

instance Ordering Bol where
  lt = prm2 ltdBolVar

instance Ordering Int where
  lt = prm2 ltdIntVar

instance Ordering Flt where
  lt = prm2 ltdFltVar

gt :: (Equality t , Ordering t , Type t) =>
      Data t -> Data t -> Data Bol
gt =  \ xx -> \ yy -> share xx (\ x ->
                      share yy (\ y ->
                      not (or (lt x y) (eql x y))))

lte :: (Equality t , Ordering t , Type t) =>
       Data t -> Data t -> Data Bol
lte = \ xx -> \ yy -> share xx (\ x ->
                      share yy (\ y ->
                      or (lt x y) (eql x y)))

gte :: (Equality t , Ordering t) => Data t -> Data t -> Data Bol
gte = \ x -> \ y -> not (lt x y)

min :: (Ordering t , Type t) => Data t -> Data t -> Data t
min xx yy = share xx (\ x ->
            share yy (\ y ->
            (lt x y) ? (x , y)))

-----------------------------------------------------------------------
-- Numeric
-----------------------------------------------------------------------
instance Num (Data Flt) where
  (+) = prm2 addFltVar
  (-) = prm2 subFltVar
  (*) = Mul
  fromInteger x = ConF (MP.fromInteger x)
  abs    = impossible
  signum = impossible

instance Num (Data Int) where
  (+) = prm2 addIntVar
  (-) = prm2 subIntVar
  (*) = Mul
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

-----------------------------------------------------------------------
-- Bitwise Operators
-----------------------------------------------------------------------

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
testBit    = \ i -> \ j -> (eql (bitAnd i (shfLft 1 j)) 0) ?
                           (false
                           ,true)

oneBits :: Data Int -> Data Int
oneBits    = \ n -> complement (shfLft (complement 0) n)

lsbs :: Data Int -> Data Int -> Data Int
lsbs       = \ k -> \ i -> bitAnd i (oneBits k)

-----------------------------------------------------------------------
-- Conversion Operators
-----------------------------------------------------------------------

i2f :: Data Int -> Data Flt
i2f = prm1 i2fVar

cis :: Data Flt -> Data Cmx
cis = prm1 cisVar

-----------------------------------------------------------------------
-- Undefined
-----------------------------------------------------------------------

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

-----------------------------------------------------------------------
-- Option Type
-----------------------------------------------------------------------

data Opt_R a = Opt_R { def :: Data Bol, val :: a }

instance Syn a => Syn (Opt_R a) where
  type InT (Opt_R a) =  (Bol, InT a)
  toExp (Opt_R b x)  =  Tpl b (toExp x)
  frmExp pp          =  let p = $shared pp in
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

-----------------------------------------------------------------------
-- Ary
-----------------------------------------------------------------------

arr :: Data Int -> (Data Int -> Data t) -> Data (Ary t)
arr = Ary

arrLen  :: Type t => Data (Ary t) -> Data Int
arrLen = Len

arrIx :: Data (Ary t) -> Data Int -> Data t
arrIx = Ind
