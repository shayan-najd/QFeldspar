module Tests.TemplateHaskell where

import QFeldspar.MyPrelude

import Language.Haskell.TH.Syntax

add :: Word32 -> Word32 -> Word32
add = (+)

dbl     :: Q (TExp (Word32 -> Word32))
dbl     = [||\ x -> add x x ||]

compose :: Q (TExp ((tb -> tc) -> (ta -> tb) -> ta -> tc))
compose = [|| \ x2 -> \ x1 -> \ x0 -> x2 (x1 x0) ||]

four   :: Q (TExp Word32)
four    = [|| ($$compose $$dbl $$dbl) 1 ||]
