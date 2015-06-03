module QFeldspar.Expression.ADTUntypedDebruijn
       (Exp(..),Fun(..)) where

import QFeldspar.MyPrelude
import QFeldspar.Variable.Plain
import qualified QFeldspar.Type.ADT as TFA

data Fun = Fun Exp

deriving instance Eq   Fun
deriving instance Show Fun

data Exp = ConI Word32
         | ConB Bool
         | ConF Float
         | Var  Var
         | Prm  Var [Exp]
         | Abs  Fun
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
         | Let  Exp Fun
         | Cmx  Exp Exp
         | Non
         | Som  Exp
         | May  Exp Exp Exp
         | Typ  TFA.Typ Exp
         | Mul  Exp Exp
         | Add  Exp Exp
         | Sub  Exp Exp
         | Eql  Exp Exp
         | Ltd  Exp Exp
         | Int  Word32
         | Mem  Exp
         | Fix  Exp

deriving instance Eq   Exp
deriving instance Show Exp
