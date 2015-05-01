module QFeldspar.Expression.GADTTyped
       (Exp(..)) where

import QFeldspar.MyPrelude
import QFeldspar.Variable.Scoped
import qualified QFeldspar.Nat.ADT as NA

data Exp :: NA.Nat -> * -> * where
  ConI :: Word32  -> Exp n t
  ConB :: Bool    -> Exp n t
  ConF :: Float   -> Exp n t
  Var  :: Var n   -> Exp n t
  Abs  :: Exp (NA.Suc n) t -> Exp n t
  App  :: t -> Exp n t -> Exp n t -> Exp n t
  Cnd  :: Exp n t -> Exp n t -> Exp n t -> Exp n t
  Whl  :: Exp n t -> Exp n t -> Exp n t -> Exp n t
  Tpl  :: Exp n t -> Exp n t -> Exp n t
  Fst  :: t -> Exp n t -> Exp n t
  Snd  :: t -> Exp n t -> Exp n t
  Ary  :: Exp n t -> Exp n t -> Exp n t
  Len  :: t -> Exp n t -> Exp n t
  Ind  :: Exp n t -> Exp n t -> Exp n t
  AryV :: Exp n t -> Exp n t -> Exp n t
  LenV :: t -> Exp n t -> Exp n t
  IndV :: Exp n t -> Exp n t -> Exp n t
  Let  :: t -> Exp n t -> Exp (NA.Suc n) t -> Exp n t
  Cmx  :: Exp n t -> Exp n t -> Exp n t
  Non  :: Exp n t
  Som  :: Exp n t -> Exp n t
  May  :: t -> Exp n t -> Exp n t -> Exp n t -> Exp n t
  Typ  :: t -> Exp n t -> Exp n t
  Mul  :: Exp n t -> Exp n t -> Exp n t
  Add  :: Exp n t -> Exp n t -> Exp n t
  Sub  :: Exp n t  -> Exp n t -> Exp n t
  Eql  :: t -> Exp n t -> Exp n t -> Exp n t
  Ltd  :: t -> Exp n t -> Exp n t -> Exp n t
  Int  :: Word32 -> Exp n t
  Mem  :: Exp n t -> Exp n t
  Fix  :: Exp n t -> Exp n t

deriving instance Eq t   => Eq   (Exp n t)
deriving instance Show t => Show (Exp n t)
deriving instance Functor        (Exp n)
deriving instance Foldable       (Exp n)
deriving instance Traversable    (Exp n)
