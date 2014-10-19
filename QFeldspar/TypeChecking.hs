module QFeldspar.TypeChecking where

import QFeldspar.Type.Herbrand
import QFeldspar.InferenceMonad
import QFeldspar.Nat.ADT

class Chk (ef :: * -> *) where
  type Cns ef :: [Nat]
  type Env ef :: * -> *
  chk :: ef (Typ (Cns ef)) -> (Env ef) (Typ (Cns ef)) ->
         InfM (Cns ef) (Typ (Cns ef))