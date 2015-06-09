
module QFeldspar.Type.ADT where

import QFeldspar.MyPrelude
import QFeldspar.Nat.ADT

data Typ =
    Wrd
  | Bol
  | Flt
  | Arr Typ Typ
  | Tpl Typ Typ
  | Ary Typ
  | Vec Typ
  | May Typ
  | Cmx
  | TVr Nat

deriving instance Eq   Typ
deriving instance Show Typ
