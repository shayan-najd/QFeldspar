module QFeldspar.Expression.Feldspar.ADTUntypedDebruijn
       (Exp(..)) where

import QFeldspar.MyPrelude
import QFeldspar.Variable.Plain
import qualified QFeldspar.Type.Feldspar.ADT as TFA

-- data Fun = Fun Exp

data Exp = ConI Int
         | ConB Bool
         | ConF Float
         | Var  Var
         | Abs  Exp
         | App  Exp Exp
         | Cnd  Exp Exp Exp
         | Whl  Exp Exp Exp
         | Tpl  Exp Exp
         | Fst  Exp
         | Snd  Exp
         | Ary  Exp Exp
         | Len  Exp
         | Ind  Exp Exp
         | AryV Exp Exp
         | LenV Exp
         | IndV Exp Exp
         | Let  Exp Exp
         | Cmx  Exp Exp
         | Non
         | Som  Exp
         | May  Exp Exp Exp
         | Typ  TFA.Typ Exp
         | Mul  Exp Exp

deriving instance Eq   Exp
deriving instance Show Exp
