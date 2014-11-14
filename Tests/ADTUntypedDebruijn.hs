module Tests.ADTUntypedDebruijn where

import QFeldspar.MyPrelude

import QFeldspar.Expression.Feldspar.ADTUntypedDebruijn
import QFeldspar.Variable.Plain
import qualified QFeldspar.Expression.Feldspar.ADTValue as V
import QFeldspar.Conversion
import QFeldspar.Expression.Feldspar.Conversions.Evaluation.ADTUntypedDebruijn ()

dbl :: Exp
dbl = Abs (Fun (App (App (Var (Suc Zro)) (Var Zro)) (Var Zro)))

compose :: Exp
compose = Abs (Fun
            (Abs (Fun
               (Abs (Fun (App (Var (Suc (Suc Zro)))
                                  (App (Var (Suc Zro))
                                           (Var Zro))))))))

four :: Exp
four = App (App (App compose dbl) dbl) (ConI 1)

test :: Bool
test = (case cnv (four , [V.addV]) of
          Rgt (V.ConI 4) -> True
          _              -> False)
