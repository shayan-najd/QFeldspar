
module QFeldspar.Type.ADT where

import QFeldspar.MyPrelude
import QFeldspar.Nat.ADT

data Typ =
    Wrd
  | Bol
  | Flt
  | Cmx
  | Int
  | Rat
  | Chr
  | Str
  | Arr Typ Typ
  | Tpl Typ Typ
  | Ary Typ
  | Vec Typ
  | May Typ
  | TVr Nat

deriving instance Eq   Typ
deriving instance Show Typ
