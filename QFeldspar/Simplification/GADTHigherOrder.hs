module QFeldspar.Simplification.GADTHigherOrder (smp,smpF) where

import QFeldspar.MyPrelude

import QFeldspar.Expression.GADTHigherOrder
import QFeldspar.Conversion
import QFeldspar.Environment.Typed
import QFeldspar.Singleton
import QFeldspar.Expression.Conversions.Lifting (cnvHOFOF,cnvFOHOF)
import qualified QFeldspar.Type.GADT as TFG
import qualified QFeldspar.Expression.GADTFirstOrder as GFO
import qualified QFeldspar.Simplification.GADTFirstOrder as GFO

smp :: forall a g.
       (HasSin TFG.Typ a , HasSin (Env TFG.Typ) g) =>
       Exp g a -> Exp g a
smp e = let g = sin :: Env TFG.Typ g
            e' :: GFO.Exp g a = frmRgtZro (cnv (e , g))
            e'' = GFO.smp e'
         in frmRgtZro (cnv (e'' , g))

smpF :: forall a b g.
        (HasSin TFG.Typ a , HasSin TFG.Typ b , HasSin (Env TFG.Typ) g) =>
        (Exp g a -> Exp g b) -> (Exp g a -> Exp g b)
smpF f = let g = sin :: Env TFG.Typ g
             e' :: GFO.Exp (a ': g) b = cnvHOFOF g f
             e'' = GFO.smp e'
         in cnvFOHOF g e''
