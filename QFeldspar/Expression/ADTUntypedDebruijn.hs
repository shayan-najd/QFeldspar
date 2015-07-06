module QFeldspar.Expression.ADTUntypedDebruijn
       (Exp(..),Fun(..)) where

import QFeldspar.MyPrelude
import QFeldspar.Variable.Plain
import qualified QFeldspar.Type.ADT as TA
import QFeldspar.Literal.ADT

data Fun = Fun Exp

deriving instance Eq   Fun
deriving instance Show Fun

data Exp = Lit  Lit
         | ConB Bool
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
         | LeT  Exp Fun
         | Cmx  Exp Exp
         | Non
         | Som  Exp
         | May  Exp Exp Exp
         | Typ  TA.Typ Exp
         | Mul  Exp Exp
         | Add  Exp Exp
         | Sub  Exp Exp
         | Eql  Exp Exp
         | Ltd  Exp Exp
         | Int  Integer
         | Rat  Rational
         | Mem  Exp
         | Fix  Exp

deriving instance Eq   Exp
deriving instance Show Exp
