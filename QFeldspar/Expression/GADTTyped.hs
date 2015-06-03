module QFeldspar.Expression.GADTTyped
       (Exp(..)) where

import QFeldspar.MyPrelude
import QFeldspar.Variable.Scoped
import qualified QFeldspar.Nat.ADT as NA

data Exp :: NA.Nat -> NA.Nat -> * -> * where
  ConI :: Word32  -> Exp n m t
  ConB :: Bool    -> Exp n m t
  ConF :: Float   -> Exp n m t
  Var  :: Var m   -> Exp n m t
  Prm  :: t -> Var n -> [Exp n m t] -> Exp n m t
  Abs  :: Exp n (NA.Suc m) t -> Exp n m t
  App  :: t -> Exp n m t -> Exp n m t -> Exp n m t
  Cnd  :: Exp n m t -> Exp n m t -> Exp n m t -> Exp n m t
  Whl  :: Exp n m t -> Exp n m t -> Exp n m t -> Exp n m t
  Tpl  :: Exp n m t -> Exp n m t -> Exp n m t
  Fst  :: t -> Exp n m t -> Exp n m t
  Snd  :: t -> Exp n m t -> Exp n m t
  Ary  :: Exp n m t -> Exp n m t -> Exp n m t
  Len  :: t -> Exp n m t -> Exp n m t
  Ind  :: Exp n m t -> Exp n m t -> Exp n m t
  AryV :: Exp n m t -> Exp n m t -> Exp n m t
  LenV :: t -> Exp n m t -> Exp n m t
  IndV :: Exp n m t -> Exp n m t -> Exp n m t
  Let  :: t -> Exp n m t -> Exp n (NA.Suc m) t -> Exp n m t
  Cmx  :: Exp n m t -> Exp n m t -> Exp n m t
  Non  :: Exp n m t
  Som  :: Exp n m t -> Exp n m t
  May  :: t -> Exp n m t -> Exp n m t -> Exp n m t -> Exp n m t
  Typ  :: t -> Exp n m t -> Exp n m t
  Mul  :: Exp n m t -> Exp n m t -> Exp n m t
  Add  :: Exp n m t -> Exp n m t -> Exp n m t
  Sub  :: Exp n m t  -> Exp n m t -> Exp n m t
  Eql  :: t -> Exp n m t -> Exp n m t -> Exp n m t
  Ltd  :: t -> Exp n m t -> Exp n m t -> Exp n m t
  Int  :: Word32 -> Exp n m t
  Mem  :: Exp n m t -> Exp n m t
  Fix  :: Exp n m t -> Exp n m t

deriving instance Eq t   => Eq   (Exp n m t)
deriving instance Show t => Show (Exp n m t)
deriving instance Functor        (Exp n m)
deriving instance Foldable       (Exp n m)
deriving instance Traversable    (Exp n m)
