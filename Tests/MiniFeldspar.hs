module Tests.MiniFeldspar where

import QFeldspar.MyPrelude

import QFeldspar.Expression.MiniFeldspar
import QFeldspar.Environment.Typed
import qualified QFeldspar.Expression.GADTValue as FGV
import QFeldspar.Variable.Typed
import QFeldspar.Conversion
import QFeldspar.Expression.Conversions.Evaluation.MiniFeldspar ()

type Add = Arr Int (Arr Int Int)
type EnvAdd = Add ': '[]

(+.) :: Exp EnvAdd Int -> Exp EnvAdd Int -> Exp EnvAdd Int
e1 +. e2 = AppV Zro (Ext e1 (Ext e2 Emp))

dbl :: Exp EnvAdd Int -> Exp EnvAdd Int
dbl x = x +. x

compose :: (Exp r tb -> Exp r tc) -> (Exp r ta -> Exp r tb)
           -> Exp r ta -> Exp r tc
compose = (.)

four :: Exp EnvAdd Int
four = (dbl . dbl) (ConI 1)

test :: Bool
test = case cnv (four
                , Ext (FGV.Exp (+)
                       :: FGV.Exp (Arr Int (Arr Int Int)))
                  Emp) of
  Rgt (FGV.Exp x) -> x == (4 :: Int)
  Lft _           -> False
