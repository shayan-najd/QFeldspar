module Tests.GADTTyped where

import QFeldspar.MyPrelude

import QFeldspar.Expression.Feldspar.GADTTyped
import QFeldspar.Variable.Scoped
import qualified QFeldspar.Type.Feldspar.ADT as TFA
import qualified QFeldspar.Expression.Feldspar.ADTValue as V
import qualified QFeldspar.Nat.ADT as NA
import QFeldspar.Environment.Scoped
import QFeldspar.Conversion
import QFeldspar.Expression.Feldspar.Conversions.Evaluation.GADTTyped ()
import QFeldspar.TypeChecking.Feldspar ()
import QFeldspar.Inference

dbl :: Exp (NA.Suc NA.Zro) TFA.Typ
dbl = Abs (App TFA.Int (App TFA.Int (Var (Suc Zro)) (Var Zro)) (Var Zro))

compose :: TFA.Typ -> TFA.Typ -> Exp n TFA.Typ
compose ta tb = Abs (Abs (Abs
  (App tb (Var (Suc (Suc Zro)))
   (App ta (Var (Suc Zro)) (Var Zro)))))

four :: Exp (NA.Suc NA.Zro) TFA.Typ
four = App TFA.Int
       (App (TFA.Arr TFA.Int TFA.Int)
        (App (TFA.Arr TFA.Int TFA.Int)
         (compose TFA.Int TFA.Int) dbl) dbl) (ConI 1)

test :: Bool
test = (case cnv (four , (Ext V.addV Emp)) of
          Rgt (V.ConI 4) -> True
          _              -> False)
       && (typChk four (Ext (TFA.Arr TFA.Int
                             (TFA.Arr TFA.Int TFA.Int)) Emp)
           ==
           Rgt TFA.Int)