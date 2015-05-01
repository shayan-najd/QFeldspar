module Tests.MiniFeldspar where

import QFeldspar.MyPrelude

import QFeldspar.Expression.MiniFeldspar
import QFeldspar.Environment.Typed
import qualified QFeldspar.Expression.GADTValue as FGV
import QFeldspar.Variable.Typed
import QFeldspar.Conversion
import QFeldspar.Expression.Conversions.Evaluation.MiniFeldspar ()

type Add = Word32 -> Word32 -> Word32
type EnvAdd = '[Add]

(+.) :: Exp EnvAdd Word32 -> Exp EnvAdd Word32 -> Exp EnvAdd Word32
e1 +. e2 = AppV Zro (Ext e1 (Ext e2 Emp))

dbl :: Exp EnvAdd Word32 -> Exp EnvAdd Word32
dbl x = x +. x

compose :: (Exp r tb -> Exp r tc) -> (Exp r ta -> Exp r tb)
           -> Exp r ta -> Exp r tc
compose = (.)

four :: Exp EnvAdd Word32
four = (dbl . dbl) (ConI 1)

test :: Bool
test = case runNamM (cnv (four
                , Ext (FGV.Exp (+)
                       :: FGV.Exp (Word32 -> Word32 -> Word32))
                  Emp)) of
  Rgt (FGV.Exp x) -> x == (4 :: Word32)
  Lft _           -> False
