module QFeldspar.CSE.GADTHigherOrder (cse,cseF) where

import QFeldspar.MyPrelude

import QFeldspar.Expression.GADTHigherOrder
import QFeldspar.Conversion
import QFeldspar.Environment.Typed
import QFeldspar.Singleton
import QFeldspar.Expression.Conversions.Lifting (cnvHOFOF,cnvFOHOF)
import qualified QFeldspar.Type.GADT as TFG
import qualified QFeldspar.Expression.GADTFirstOrder as GFO
import qualified QFeldspar.CSE.GADTFirstOrder as GFO

cse :: forall a g.
       (HasSin TFG.Typ a , HasSin (Env TFG.Typ) g) =>
       Exp g a -> Exp g a
cse e = let g = sin :: Env TFG.Typ g
            e' :: GFO.Exp g a = frmRgtZro (cnv (e , g))
            e'' = GFO.cse e'
         in frmRgtZro (cnv (e'' , g))

cseF :: forall a b g.
        (HasSin TFG.Typ a , HasSin TFG.Typ b , HasSin (Env TFG.Typ) g) =>
        (Exp g a -> Exp g b) -> (Exp g a -> Exp g b)
cseF f = let g = sin :: Env TFG.Typ g
             e' :: GFO.Exp (a ': g) b = cnvHOFOF g f
             e'' = GFO.cse e'
         in cnvFOHOF g e''
