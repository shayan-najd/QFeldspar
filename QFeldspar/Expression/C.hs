module QFeldspar.Expression.C
    (Var,Func(..),Stmt(..),Exp(..))where

import QFeldspar.MyPrelude

import qualified QFeldspar.Type.ADT as TA

type Var = (String , TA.Typ)

data Func = Func TA.Typ String [Var] [Stmt]

data Stmt =
   If  Exp [Stmt] [Stmt]
 | Whl Exp [Stmt]
 | Assign String Exp
 | Declare Var
 | Return  Exp
 deriving Eq

data Exp =
   Var String
 | Wrd Word32
 | Flt Float
 | App String [Exp]
 deriving Eq
