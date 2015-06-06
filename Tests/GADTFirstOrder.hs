module Tests.GADTFirstOrder where

import QFeldspar.MyPrelude

import QFeldspar.Expression.GADTFirstOrder
import QFeldspar.Variable.Typed
import QFeldspar.Conversion
import QFeldspar.Expression.Conversions.Evaluation.GADTFirstOrder ()
import qualified QFeldspar.Expression.GADTValue as FGV
import QFeldspar.Singleton
import QFeldspar.Environment.Typed

import qualified QFeldspar.Type.GADT as TG

dbl :: Exp '[Word32 -> Word32 -> Word32] '[] (Word32 -> Word32)
dbl = -- Abs (App (App (Var (Suc Zro)) (Var Zro)) (Var Zro))
       Abs (Prm Zro (Ext (Var Zro) (Ext (Var Zro) Emp)))

compose :: (HasSin TG.Typ ta , HasSin TG.Typ tb , HasSin TG.Typ tc) =>
           Exp s g ((tb -> tc) -> ((ta -> tb) -> (ta -> tc)))
compose = Abs (Abs (Abs
                    (App (Var (Suc (Suc Zro)))
                     (App (Var (Suc Zro)) (Var Zro)))))

four :: Exp '[Word32 -> Word32 -> Word32] '[] Word32
four = App (App (App compose dbl) dbl) (ConI 1)

test :: Bool
test = case runNamM (cnv (four
                , (Ext (FGV.Exp (+) :: FGV.Exp (Word32 -> Word32 -> Word32)) Emp, Emp :: Env FGV.Exp '[])))
       of
  Rgt (FGV.Exp x) -> x == 4
  Lft _           -> False
