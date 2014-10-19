module Tests.GADTFirstOrder where

import QFeldspar.MyPrelude

import QFeldspar.Expression.Feldspar.GADTFirstOrder
import QFeldspar.Variable.Typed
import QFeldspar.Conversion
import QFeldspar.Expression.Feldspar.Conversions.Evaluation.GADTFirstOrder ()
import qualified QFeldspar.Expression.Feldspar.GADTValue as FGV
import QFeldspar.Singleton
import QFeldspar.Environment.Typed

import qualified QFeldspar.Type.Feldspar.GADT as TFG

dbl :: Exp (Arr Int (Arr Int Int) ': '[])
       (Arr Int Int)
dbl = Abs (App (App (Var (Suc Zro)) (Var Zro)) (Var Zro))

compose :: (HasSin TFG.Typ ta , HasSin TFG.Typ tb , HasSin TFG.Typ tc) =>
           Exp r (Arr (Arr tb tc) (Arr (Arr ta tb)
                   (Arr ta tc)))
compose = Abs (Abs (Abs
                    (App (Var (Suc (Suc Zro)))
                     (App (Var (Suc Zro)) (Var Zro)))))

four :: Exp (Arr Int (Arr Int Int) ': '[]) Int
four = App (App (App compose dbl) dbl) (ConI 1)

test :: Bool
test = case cnv (four
                , Ext
                  (FGV.Exp (+)
                   :: FGV.Exp (Arr Int (Arr Int Int))) Emp)
       of
  Rgt (FGV.Exp x) -> x == 4
  Lft _           -> False
