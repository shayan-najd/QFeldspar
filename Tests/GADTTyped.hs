module Tests.GADTTyped where

import QFeldspar.MyPrelude

import QFeldspar.Expression.GADTTyped
import QFeldspar.Variable.Scoped
import qualified QFeldspar.Type.ADT as TA
import qualified QFeldspar.Expression.ADTValue as V
import qualified QFeldspar.Nat.ADT as NA
import QFeldspar.Environment.Scoped
import QFeldspar.Conversion
import QFeldspar.Expression.Conversions.Evaluation.GADTTyped ()
import QFeldspar.Inference

dbl :: Exp ('NA.Suc 'NA.Zro) 'NA.Zro TA.Typ
dbl = Abs (Prm [TA.Wrd ,TA.Wrd] Zro
                   [Var Zro , Var Zro])

compose :: TA.Typ -> TA.Typ -> Exp n m TA.Typ
compose ta tb = Abs (Abs (Abs
  (App tb (Var (Suc (Suc Zro)))
   (App ta (Var (Suc Zro)) (Var Zro)))))

four :: Exp ('NA.Suc 'NA.Zro) 'NA.Zro TA.Typ
four = App TA.Wrd
       (App (TA.Arr TA.Wrd TA.Wrd)
        (App (TA.Arr TA.Wrd TA.Wrd)
         (compose TA.Wrd TA.Wrd) dbl) dbl) (Int 1)

test :: Bool
test = (case runNamM (cnv (four , (Ext (V.toExp ((+) :: Word32 -> Word32 -> Word32)) Emp , Emp :: Env 'NA.Zro V.Exp))) of
          Rgt (V.frmExp -> Rgt (4 :: Word32)) -> True
          _                           -> False)
       && (runNamM (typChk four (Ext (TA.Arr TA.Wrd
                             (TA.Arr TA.Wrd TA.Wrd)) Emp , Emp :: Env 'NA.Zro TA.Typ))
           ==
           Rgt TA.Wrd)
