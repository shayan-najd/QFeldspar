module QFeldspar.Expression.Feldspar.GADTUntypedDebruijn where

import QFeldspar.MyPrelude

import QFeldspar.Variable.Scoped

import qualified QFeldspar.Nat.ADT as NA

data Exp :: NA.Nat -> * where
  ConI :: Int     -> Exp n
  ConB :: Bool    -> Exp n
  ConF :: Float   -> Exp n
  Var  :: Var n   -> Exp n
  Abs  :: Exp (NA.Suc n)  -> Exp n
  App  :: Exp n -> Exp n -> Exp n
  Cnd  :: Exp n -> Exp n -> Exp n -> Exp n
  Whl  :: Exp (NA.Suc n) -> Exp (NA.Suc n) -> Exp n -> Exp n
  Tpl  :: Exp n -> Exp n -> Exp n
  Fst  :: Exp n -> Exp n
  Snd  :: Exp n -> Exp n
  Ary  :: Exp n -> Exp (NA.Suc n) -> Exp n
  Len  :: Exp n -> Exp n
  Ind  :: Exp n -> Exp n -> Exp n
  Let  :: Exp n -> Exp (NA.Suc n) -> Exp n
  Cmx  :: Exp n -> Exp n -> Exp n
  Non  :: Exp n
  Som  :: Exp n -> Exp n
  May  :: Exp n -> Exp n -> Exp (NA.Suc n) -> Exp n
  deriving Eq