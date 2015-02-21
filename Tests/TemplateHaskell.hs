module Tests.TemplateHaskell where

import QFeldspar.MyPrelude

import Language.Haskell.TH.Syntax

add :: Int -> Int -> Int
add = (+)

dbl     :: Q (TExp (Int -> Int))
dbl     = [||\ x -> add x x ||]

compose :: Q (TExp ((tb -> tc) -> (ta -> tb) -> ta -> tc))
compose = [|| \ x2 -> \ x1 -> \ x0 -> x2 (x1 x0) ||]

four   :: Q (TExp Int)
four    = [|| ($$compose $$dbl $$dbl) 1 ||]
