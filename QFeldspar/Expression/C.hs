module QFeldspar.Expression.C
    (Var,Func(..),Stmt(..),Exp(..))where

import QFeldspar.MyPrelude

import qualified QFeldspar.Type.ADT as TFA

type Var = (String , TFA.Typ)

data Func = Func String [Var] [Stmt]

data Stmt =
   If  Exp [Stmt] [Stmt]
 | Whl Exp [Stmt]
 | Assign String Exp
 | Declare Var
 deriving Eq

data Exp =
   Var String
 | Num Int
 | Flt Float
 | App String [Exp]
 deriving Eq
