module Tests.ADTUntypedDebruijn where

import QFeldspar.MyPrelude

import QFeldspar.Expression.ADTUntypedDebruijn
import QFeldspar.Variable.Plain
import qualified QFeldspar.Expression.ADTValue as V
import QFeldspar.Conversion
import QFeldspar.Expression.Conversions.Evaluation.ADTUntypedDebruijn ()

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
test = (case runNamM $ cnv (four , [V.lft ((+) :: Int -> Int -> Int)]) of
          Rgt (V.colft -> Rgt (4 :: Int)) -> True
          _                               -> False)
