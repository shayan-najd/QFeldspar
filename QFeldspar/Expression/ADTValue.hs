module QFeldspar.Expression.ADTValue
    (Exp
    ,conI,conB,conF,var,abs,app,cnd,whl,tpl,fst,snd,ary,len,ind,leT
    ,cmx,typ,mul,add,sub,eql,ltd,int,mem
    ,Lft(..),CoLft(..)) where

import QFeldspar.MyPrelude hiding (abs,fst,snd,may,som,non,cmx,tpl,cnd)
import qualified QFeldspar.MyPrelude as MP
import qualified QFeldspar.Type.ADT as TFA

data Exp = ConI Int
         | ConB Bol
         | ConF Flt
         | Abs (Exp -> ErrM Exp)
         | Tpl (Exp , Exp)
         | Ary (Ary Exp)
         | Cmx Cmx

class Lft t where
  lft :: t -> Exp

instance Lft Exp where
  lft = id

instance Lft Int where
  lft = ConI

instance Lft Bool where
  lft = ConB

instance Lft Float where
  lft = ConF

instance (CoLft a , Lft b) => Lft (a -> b) where
  lft f = Abs (fmap (lft . f) . colft)

instance (Lft a , Lft b) => Lft (a , b) where
  lft (x , y) = Tpl (lft x , lft y)

instance Lft a => Lft (Array Int a) where
  lft a = Ary (fmap lft a)

instance Lft (Complex Float) where
  lft = Cmx

class CoLft t where
  colft :: Exp -> ErrM t

instance CoLft Int where
  colft (ConI i) = return i
  colft _        = badTypValM

instance CoLft Bool where
  colft (ConB b) = return b
  colft _        = badTypValM

instance CoLft Float where
  colft (ConF f) = return f
  colft _        = badTypValM

instance (Lft a , CoLft b) => CoLft (a -> ErrM b) where
  colft (Abs f)  = return (\ e -> colft =<< (f (lft e)))
  colft _        = badTypValM

instance (CoLft a , CoLft b) => CoLft (a , b) where
  colft (Tpl (x , y)) = ((,)) <$> colft x <*> colft y
  colft _             = badTypValM

instance CoLft a => CoLft (Array Int a) where
  colft (Ary x)  = mapM colft x
  colft _        = badTypValM

instance CoLft (Complex Float) where
  colft (Cmx c)  = return c
  colft _        = badTypVal

class ToHsk t where
  toHsk :: Exp -> ErrM t

instance ToHsk Int where
  toHsk (ConI i) = return i
  toHsk _        = badTypValM

instance ToHsk Bool where
  toHsk (ConB b) = return b
  toHsk _        = badTypValM

instance ToHsk Float where
  toHsk (ConF f) = return f
  toHsk _        = badTypValM

instance ToHsk (Exp -> ErrM Exp) where
  toHsk (Abs f)  = return f
  toHsk _        = badTypValM

instance ToHsk (Exp , Exp) where
  toHsk (Tpl p ) = return p
  toHsk _        = badTypValM

instance ToHsk (Array Int Exp) where
  toHsk (Ary x)  = return x
  toHsk _        = badTypValM

instance ToHsk (Complex Float) where
  toHsk (Cmx c)  = return c
  toHsk _        = badTypVal

prm0 :: Lft a => a -> ErrM Exp
prm0 = return . lft

{-
prm1 :: (ToHsk a , Lft b) => (a -> b) -> Exp -> ErrM Exp
prm1 f x = do x' <- toHsk x
              return (lft (f x'))

prm2 :: (ToHsk a,ToHsk b,Lft c) => (a -> b -> c) -> Exp -> Exp -> ErrM Exp
prm2 f x y = do x' <- toHsk x
                y' <- toHsk y
                return (lft (f x' y'))
-}

var :: a -> NamM ErrM a
var = return

conI :: Int -> NamM ErrM Exp
conI = lift . prm0

conB :: Bool -> NamM ErrM Exp
conB = lift . prm0

conF :: Float -> NamM ErrM Exp
conF = lift . prm0

abs :: (Exp -> Exp) -> NamM ErrM Exp
abs f = return (Abs (return . f))

app :: Exp -> Exp -> NamM ErrM Exp
app vf va = do vf' <- lift (toHsk vf)
               lift (vf' va)

cnd :: Exp -> Exp -> Exp -> NamM ErrM Exp
cnd vc v1 v2 = do vc' <- lift (toHsk vc)
                  return (if vc' then v1 else v2)

whl :: Exp -> Exp -> Exp -> NamM ErrM Exp
whl fc fb v = do fc' <- lift (toHsk fc)
                 fb' <- lift (toHsk fb)
                 whileM ((lift . colft =<<) . (lift . fc')) (lift . fb') v

fst :: Exp -> NamM ErrM Exp
fst (Tpl p) = return (MP.fst p)
fst _       = badTypValM

snd :: Exp -> NamM ErrM Exp
snd (Tpl p) = return (MP.snd p)
snd _       = badTypValM

tpl :: Exp -> Exp -> NamM ErrM Exp
tpl vf vs = return (lft (vf , vs))

ary :: Exp -> Exp -> NamM ErrM Exp
ary (ConI l) (Abs vf) = fmap lft (sequence (mkArr l ((lift . vf) . ConI)))
ary _        _        = badTypValM

len :: Exp -> NamM ErrM Exp
len (Ary a) = return (lft (lnArr a))
len _       = badTypValM

ind :: Exp -> Exp -> NamM ErrM Exp
ind (Ary a) (ConI i) = return (a ! i)
ind _       _        = badTypValM

cmx :: Exp -> Exp -> NamM ErrM Exp
cmx fr fi = do fr' <- lift (toHsk fr)
               fi' <- lift (toHsk fi)
               return (Cmx (fr' :+ fi'))

leT :: Exp -> (Exp -> Exp) -> NamM ErrM Exp
leT e f = return (f e)

typ :: TFA.Typ -> Exp -> NamM ErrM Exp
typ _ = return

mul :: Exp -> Exp -> NamM ErrM Exp
mul (ConI i) (ConI i') = return (lft (i * i'))
mul (ConF f) (ConF f') = return (lft (f * f'))
mul _        _         = badTypValM

add :: Exp -> Exp -> NamM ErrM Exp
add (ConI i) (ConI i') = return (lft (i + i'))
add (ConF f) (ConF f') = return (lft (f + f'))
add _        _         = badTypValM

sub :: Exp -> Exp -> NamM ErrM Exp
sub (ConI i) (ConI i') = return (lft (i - i'))
sub (ConF f) (ConF f') = return (lft (f - f'))
sub _        _         = badTypValM

eql :: Exp -> Exp -> NamM ErrM Exp
eql (ConI i) (ConI i') = return (lft (i == i'))
eql (ConF f) (ConF f') = return (lft (f == f'))
eql _        _         = badTypValM

ltd :: Exp -> Exp -> NamM ErrM Exp
ltd (ConI i) (ConI i') = return (lft (i < i'))
ltd (ConF f) (ConF f') = return (lft (f < f'))
ltd _        _         = badTypValM

int :: Int -> NamM ErrM Exp
int = lift . prm0

mem :: Exp -> NamM ErrM Exp
mem = return
