module QFeldspar.Type.ADT where

import QFeldspar.MyPrelude

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

deriving instance Eq   Typ
deriving instance Show Typ
