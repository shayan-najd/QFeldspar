module Tests.ADTUntypedDebruijn where

import QFeldspar.MyPrelude

import QFeldspar.Expression.ADTUntypedDebruijn
import QFeldspar.Variable.Plain
import qualified QFeldspar.Expression.ADTValue as V
import QFeldspar.Conversion
import QFeldspar.Expression.Conversions.Evaluation.ADTUntypedDebruijn ()

dbl :: Exp
dbl = Abs (Fun (Prm Zro [Var Zro,Var Zro]))

compose :: Exp
compose = Abs (Fun
            (Abs (Fun
               (Abs (Fun (App (Var (Suc (Suc Zro)))
                                  (App (Var (Suc Zro))
                                           (Var Zro))))))))

four :: Exp
four = App (App (App compose dbl) dbl) (ConI 1)

test :: Bool
test = (case runNamM (cnv (four , ([V.toExp ((+) :: Word32 -> Word32 -> Word32)],[] :: [V.Exp]))) of
          Rgt (V.frmExp -> Rgt (4 :: Word32)) -> True
          _                               -> False)
