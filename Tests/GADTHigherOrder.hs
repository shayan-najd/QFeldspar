module Tests.GADTHigherOrder where

import QFeldspar.MyPrelude

import QFeldspar.Expression.GADTHigherOrder
import QFeldspar.Variable.Typed
import QFeldspar.Conversion as E
import QFeldspar.Expression.Conversions.Evaluation.GADTHigherOrder ()
import qualified QFeldspar.Expression.GADTValue as FGV
import QFeldspar.Singleton
import QFeldspar.Type.GADT
import QFeldspar.Environment.Typed

dbl :: Exp '[Word32 -> Word32 -> Word32] (Word32 -> Word32)
dbl = Abs (\ x -> (App (App (Var Zro) x) x))

compose :: (HasSin Typ ta , HasSin Typ tb , HasSin Typ tc) =>
           Exp r ((tb -> tc) -> ((ta -> tb) -> (ta -> tc)))
compose = Abs (\ g -> Abs (\ f -> Abs
                    (\ x -> App g (App f x))))

four :: Exp '[Word32 -> Word32 -> Word32] Word32
four = App (App (App compose dbl) dbl) (ConI 1)

test :: Bool
test = case runNamM (cnv ( four
                , Ext (FGV.Exp (+)
                       :: FGV.Exp (Word32 -> Word32 -> Word32))
                  Emp)) of
  Rgt (FGV.Exp x) -> x == 4
  Lft _           -> False
