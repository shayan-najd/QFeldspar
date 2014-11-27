module QFeldspar.Expression.ADTUntypedNamed
       (Exp(..)) where

import QFeldspar.MyPrelude
import qualified QFeldspar.Type.ADT as TFA

data Exp x = ConI Int
           | ConB Bool
           | ConF Float
           | Var x
           | Abs (x , Exp x)
           | App (Exp x) (Exp x)
           | Cnd (Exp x) (Exp x) (Exp x)
           | Whl (Exp x) (Exp x) (Exp x)
           | Tpl (Exp x) (Exp x)
           | Fst (Exp x)
           | Snd (Exp x)
           | Ary (Exp x) (Exp x)
           | Len (Exp x)
           | Ind (Exp x) (Exp x)
           | AryV (Exp x) (Exp x)
           | LenV (Exp x)
           | IndV (Exp x) (Exp x)
           | Let (Exp x) (x , Exp x)
           | Cmx (Exp x) (Exp x)
           | Non
           | Som (Exp x)
           | May (Exp x) (Exp x) (Exp x)
           | Typ TFA.Typ (Exp x)
           | Mul (Exp x) (Exp x)
           | Int Int

deriving instance Eq x   => Eq   (Exp x)
deriving instance Show x => Show (Exp x)
deriving instance Functor     Exp
deriving instance Foldable    Exp
deriving instance Traversable Exp