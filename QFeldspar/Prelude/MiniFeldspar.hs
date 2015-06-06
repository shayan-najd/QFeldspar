module QFeldspar.Prelude.MiniFeldspar
  (Dp,Syn(..),toExpF,frmExpF,Type,Num,EqE,OrdE,Fractional,conF,
   Word32,Float,Bool,pattern TrueE,pattern FalseE,(?),while,fst,snd,
   Ary,mkArr,lnArr,ixArr,Vec(..),share,Complex,pattern (:+.),
   Opt,some,none,option,
   (*),(+),(-),(==.),(<.),save,
   realPartE,imagPartE,divE,(/),(.&..),(.|..),xorE,shfRgtE,shfLftE,
   complementE,i2fE,cisE,ilog2E,sqrtE,hashTableE,
   trmEql,trmEqlF) where

import QFeldspar.MyPrelude (Word32,Float,Complex,Ary,Bool(..),Num(..)
       ,Monad(..),fst,snd,Fractional(..),impossible,(.))
import qualified QFeldspar.MyPrelude as MP

import QFeldspar.Expression.MiniFeldspar
import qualified QFeldspar.Expression.Utils.Equality.MiniFeldspar as MFS
import qualified QFeldspar.Type.GADT    as TG

import QFeldspar.Singleton
import QFeldspar.Environment.Typed (Env(Emp,Ext))
import QFeldspar.Prelude.Environment
import QFeldspar.Prelude.HaskellEnvironment hiding (cis,ilog2,i2f)
import qualified QFeldspar.Variable.Typed as VT
-- import QFeldspar.Expression.Utils.MiniFeldspar (shared)

prm0 :: VT.Var Prelude ('[] TG.:-> t) -> Dp t
prm0 v = Prm v Emp

prm1 :: (TG.Type t1) =>
        VT.Var Prelude ('[t1] TG.:-> t) -> Dp t1 -> Dp t
prm1 v e = Prm v (Ext e Emp)

prm2 :: (TG.Type t1,TG.Type t2) =>
        VT.Var Prelude (('[t1, t2]) TG.:-> t) -> Dp t1 -> Dp t2 -> Dp t
prm2 v e1 e2 = Prm v (Ext e1 (Ext e2 Emp))

trmEql ::  HasSin TG.Typ a => Dp a -> Dp a -> MP.Bool
trmEql  = MFS.eql

trmEqlF :: (HasSin TG.Typ a , HasSin TG.Typ b) =>
           (Dp a -> Dp b) -> (Dp a -> Dp b) -> MP.Bool
trmEqlF = MFS.eqlF

----------
type Dp t = Exp Prelude t

conF :: Float -> Dp Float
conF = ConF

class Type (InT a) => Syn a where
  type InT a :: *
  toExp  :: a -> Dp (InT a)
  frmExp :: Dp (InT a) -> a

instance Type a => Syn (Dp a) where
  type InT (Dp a) = a
  toExp  x = x
  frmExp x = x

toExpF :: (Syn a , Syn b) => (a -> b) -> Dp (InT a) -> Dp (InT b)
toExpF f = toExp . f . frmExp

frmExpF :: (Syn a , Syn b) => (Dp (InT a) -> Dp (InT b)) -> a -> b
frmExpF f = frmExp . f . toExp

type Type t = HasSin TG.Typ t

pattern TrueE = ConB MP.True
pattern FalseE = ConB MP.False

(?) :: Syn a => Dp Bool -> (a , a) -> a
c ? (t , e) = frmExp (Cnd c (toExp t) (toExp e))

while :: Syn a => (a -> Dp Bool) -> (a -> a) -> a -> a
while c b i = frmExp (Whl (toExpF c) (toExpF b) (toExp i))

instance (Syn a , Syn b) => Syn (a , b) where
    type InT (a , b) = (InT a , InT b)
    toExp (x , y)    = Tpl (toExp x) (toExp y)
    frmExp ee        = let e = ee in
                       (frmExp (Fst e) , frmExp (Snd e))

mkArr :: Dp Word32 -> (Dp Word32 -> Dp t) -> Dp (Ary t)
mkArr = Ary

lnArr  :: Type t => Dp (Ary t) -> Dp Word32
lnArr = Len

ixArr :: Dp (Ary t) -> Dp Word32 -> Dp t
ixArr = Ind

data Vec t = Vec (Dp Word32) (Dp Word32 -> t)

instance Syn a => Syn (Vec a) where
  type InT (Vec a)  = Ary (InT a)
  toExp  (Vec l f)  = Ary l (\ i -> toExp (f i))
  frmExp aa         = let a = aa in
                      Vec (Len a) (\ i -> frmExp (Ind a i))

pattern x :+. y = Cmx x y

save :: Dp a -> Dp a
save = Mem

class Syn a => Undef a where
  undef :: a

instance Undef (Dp Bool) where
  undef = FalseE

instance Undef (Dp Word32) where
  undef = 0

instance Undef (Dp Float) where
  undef = 0

instance (Undef a, Undef b) => Undef (a,b) where
  undef = (undef, undef)

data Opt_R a = Opt_R { def :: Dp Bool, val :: a }

instance Syn a => Syn (Opt_R a) where
  type InT (Opt_R a) =  (Bool, InT a)
  toExp (Opt_R b x)  =  Tpl b (toExp x)
  frmExp pp          =  let p = pp in
                        Opt_R (Fst p) (frmExp (Snd p))

some_R            ::  a -> Opt_R a
some_R x          =   Opt_R TrueE x

none_R            ::  Undef a => Opt_R a
none_R            =   Opt_R FalseE undef

option_R          ::  Syn b => b -> (a -> b) -> Opt_R a -> b
option_R d f o    =   def o ? (f (val o), d)

newtype Opt a = O { unO :: forall b . Undef b =>
                           ((a -> Opt_R b) -> Opt_R b) }

instance Monad Opt where
  return x    =  O (\g -> g x)
  m >>= k     =  O (\g -> unO m (\x -> unO (k x) g))

instance Undef a => Syn (Opt a) where
  type InT (Opt a) =  (Bool, InT a)
  frmExp           =  lift . frmExp
  toExp            =  toExp . lower

lift          ::  Opt_R a -> Opt a
lift o        =   O (\g -> Opt_R  (def o ? (def (g (val o)), FalseE))
                                  (def o ? (val (g (val o)), undef)))

lower         ::  Undef a => Opt a -> Opt_R a
lower m       =   unO m some_R

some          ::  a -> Opt a
some a        =   lift (some_R a)

none          ::  Undef a => Opt a
none          =   lift none_R

option        ::  (Undef a, Undef b) => b -> (a -> b) -> Opt a -> b
option d f o  =   option_R d f (lower o)

instance Num (Dp Float) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  fromInteger x = ConF (MP.fromInteger x)
  abs    = impossible
  signum = impossible

instance Num (Dp Word32) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  fromInteger x = ConI (MP.fromInteger x)
  abs    = impossible
  signum = impossible

instance Num (Dp (Complex Float)) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  fromInteger x = Cmx (ConF (MP.fromInteger x)) 0.0
  abs    = impossible
  signum = impossible

infix 4 ==.
class EqE t where
  (==.) :: Dp t -> Dp t -> Dp Bool

instance EqE Bool where
  (==.) = Eql

instance EqE Word32 where
  (==.) = Eql

instance EqE Float where
  (==.) = Eql

infix 4 <.
class OrdE t where
  (<.) :: Dp t -> Dp t -> Dp Bool

instance OrdE Bool where
  (<.) = Ltd

instance OrdE Word32 where
  (<.) = Ltd

instance OrdE Float where
  (<.) = Ltd

share :: (Type (InT tl) , Syn tl , Syn tb) =>
         tl -> (tl -> tb) -> tb
share e f = frmExp (LeT (toExp e) (toExp . f . frmExp))

realPartE :: Dp (Complex Float) -> Dp Float
realPartE = prm1 realPartVar

imagPartE :: Dp (Complex Float) -> Dp Float
imagPartE = prm1 imagPartVar

divE :: Dp Word32 -> Dp Word32 -> Dp Word32
divE = prm2 divWrdVar

instance Fractional (Dp Float) where
  (/) = prm2 divFltVar
  fromRational r = ConF (fromRational r)

infixl 7 .&..
(.&..) :: Dp Word32 -> Dp Word32 -> Dp Word32
(.&..) = prm2 andWrdVar

infixl 7 .|..
(.|..)  :: Dp Word32 -> Dp Word32 -> Dp Word32
(.|..)  = prm2 orWrdVar

xorE :: Dp Word32 -> Dp Word32 -> Dp Word32
xorE = prm2 xorWrdVar

shfRgtE :: Dp Word32 -> Dp Word32 -> Dp Word32
shfRgtE = prm2 shrWrdVar

shfLftE :: Dp Word32 -> Dp Word32 -> Dp Word32
shfLftE = prm2 shlWrdVar

complementE :: Dp Word32 -> Dp Word32
complementE = prm1 cmpWrdVar

i2fE :: Dp Word32 -> Dp Float
i2fE = prm1 i2fVar

cisE :: Dp Float -> Dp (Complex Float)
cisE = prm1 cisVar

ilog2E :: Dp Word32 -> Dp Word32
ilog2E = prm1 ilog2Var

hashTableE :: Dp (Ary Word32)
hashTableE = prm0 hshTblVar

sqrtE :: Dp Float -> Dp Float
sqrtE = prm1 sqrtFltVar
