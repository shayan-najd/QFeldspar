module Tests.GADTTyped where

import QFeldspar.MyPrelude

import QFeldspar.Expression.GADTTyped
import QFeldspar.Variable.Scoped
import qualified QFeldspar.Type.ADT as TFA
import qualified QFeldspar.Expression.ADTValue as V
import qualified QFeldspar.Nat.ADT as NA
import QFeldspar.Environment.Scoped
import QFeldspar.Conversion
import QFeldspar.Expression.Conversions.Evaluation.GADTTyped ()
import QFeldspar.Inference

dbl :: Exp (NA.Suc NA.Zro) NA.Zro TFA.Typ
dbl = Abs (Prm (TFA.Arr TFA.Wrd (TFA.Arr TFA.Wrd TFA.Wrd)) Zro
                   [Var Zro , Var Zro])

compose :: TFA.Typ -> TFA.Typ -> Exp n m TFA.Typ
compose ta tb = Abs (Abs (Abs
  (App tb (Var (Suc (Suc Zro)))
   (App ta (Var (Suc Zro)) (Var Zro)))))

four :: Exp (NA.Suc NA.Zro) NA.Zro TFA.Typ
four = App TFA.Wrd
       (App (TFA.Arr TFA.Wrd TFA.Wrd)
        (App (TFA.Arr TFA.Wrd TFA.Wrd)
         (compose TFA.Wrd TFA.Wrd) dbl) dbl) (ConI 1)

test :: Bool
test = (case runNamM (cnv (four , (Ext (V.lft ((+) :: Word32 -> Word32 -> Word32)) Emp , Emp :: Env 'NA.Zro V.Exp))) of
          Rgt (V.colft -> Rgt (4 :: Word32)) -> True
          _                           -> False)
       && (runNamM (typChk four (Ext (TFA.Arr TFA.Wrd
                             (TFA.Arr TFA.Wrd TFA.Wrd)) Emp , Emp :: Env 'NA.Zro TFA.Typ))
           ==
           Rgt TFA.Wrd)
