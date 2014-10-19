module Tests.GADTUntypedDebruijn where

import QFeldspar.MyPrelude

import QFeldspar.Expression.Feldspar.GADTUntypedDebruijn
import QFeldspar.Variable.Scoped
import qualified QFeldspar.Nat.ADT                      as NA
import qualified QFeldspar.Expression.Feldspar.ADTValue as V
import QFeldspar.Conversion
import QFeldspar.Expression.Feldspar.Conversions.Evaluation.GADTUntypedDebruijn ()
import QFeldspar.Environment.Scoped

dbl :: Exp (NA.Suc NA.Zro)
dbl = Abs (App (App (Var (Suc Zro)) (Var Zro)) (Var Zro))

compose :: Exp (NA.Suc NA.Zro)
compose = Abs (Abs (Abs (App (Var (Suc (Suc Zro))) (App (Var (Suc Zro))
                                                    (Var Zro)))))

four :: Exp (NA.Suc NA.Zro)
four = App (App (App compose  dbl) dbl) (ConI 1)

test :: Bool
test = (case cnv (four , Ext V.addV Emp) of
          Rgt (V.ConI 4) -> True
          _              -> False)

