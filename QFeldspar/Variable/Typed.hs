module QFeldspar.Variable.Typed
  (Var(..),prd,inc,incM,eqlVar) where

import QFeldspar.MyPrelude

data Var :: [k] -> k -> * where
  Zro :: Var (t ': r) t
  Suc :: Var r tp -> Var (t ': r) tp

deriving instance Eq   (Var e t)
deriving instance Ord  (Var e t)

eqlVar :: Var g a -> Var g b -> Bool
eqlVar Zro    Zro       = True
eqlVar Zro     (Suc _)  = False
eqlVar (Suc _) Zro      = False
eqlVar (Suc v) (Suc v') = eqlVar v v'

int :: Var r t -> Word32
int Zro     = 0
int (Suc x) = 1 + int x

instance Show (Var r t) where
  show v = "$(nat "++show (int v)++" \"\")"

prd :: Var (t' ': r) t -> Var r t
prd (Suc x) = x
prd _       = impossible

inc :: (forall t'. Var r t' -> Var r' t') ->
       Var (ta ': r) t -> Var (ta ': r') t
inc _ Zro     = Zro
inc f (Suc x) = Suc (f x)

incM :: (Applicative m , Monad m) =>
       (forall t'. Var r t' -> m (Var r' t')) ->
       Var (ta ': r) t -> m (Var (ta ': r') t)
incM _ Zro     = pure Zro
incM f (Suc x) = Suc <$> f x
