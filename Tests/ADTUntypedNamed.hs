module Tests.ADTUntypedNamed where

import QFeldspar.MyPrelude

import QFeldspar.Expression.ADTUntypedNamed
import qualified QFeldspar.Expression.ADTValue as V
import QFeldspar.Conversion
import QFeldspar.Expression.Conversions.Evaluation.ADTUntypedNamed ()
import qualified Language.Haskell.TH.Syntax as TH

type Var = TH.Name

x0 :: Var
x0 = TH.mkName  "x0"

x1 :: Var
x1 = TH.mkName  "x1"

x2 :: Var
x2 = TH.mkName  "x2"

dbl :: Exp Var
dbl = Abs (x0 , (App (App (Var (stripNameSpace '(+))) (Var x0)) (Var x0)))

compose :: Exp Var
compose = Abs (x2 , (Abs (x1 , (Abs (x0 , (App (Var x2) (App (Var x1) (Var x0))))))))

four :: Exp Var
four = App (App (App compose dbl) dbl) (ConI 1)

test :: Bool
test = (case cnv (four , [((stripNameSpace '(+)) , V.addV)]) of
          Rgt (V.ConI 4) -> True
          _              -> False)
