module QFeldspar.Expression.Utils.Equality.GADTHigherOrder (eql,eqlF) where

import QFeldspar.MyPrelude

import QFeldspar.Expression.GADTHigherOrder
import QFeldspar.Conversion
import QFeldspar.Environment.Typed
import QFeldspar.Singleton
import QFeldspar.Expression.Conversions.Lifting (cnvHOFOF)
import qualified QFeldspar.Type.GADT as TFG
import qualified QFeldspar.Expression.GADTFirstOrder as GFO
import qualified QFeldspar.Expression.Utils.Equality.GADTFirstOrder as GFO

eql :: forall a g.
       (HasSin TFG.Typ a , HasSin (Env TFG.Typ) g) =>
       Exp g a -> Exp g a -> Bool
eql e1 e2 = let g = sin :: Env TFG.Typ g
                e1' :: GFO.Exp g a = frmRgtZro (cnv (e1 , g))
                e2' :: GFO.Exp g a = frmRgtZro (cnv (e2 , g))
            in GFO.eql e1' e2'

eqlF :: forall a b g.
        (HasSin TFG.Typ a , HasSin TFG.Typ b , HasSin (Env TFG.Typ) g) =>
        (Exp g a -> Exp g b) -> (Exp g a -> Exp g b) -> Bool
eqlF f1 f2 = let g = sin :: Env TFG.Typ g
                 f1' :: GFO.Exp (a ': g) b = cnvHOFOF g f1
                 f2' :: GFO.Exp (a ': g) b = cnvHOFOF g f2
             in GFO.eql f1' f2'
