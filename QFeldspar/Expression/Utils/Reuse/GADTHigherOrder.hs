module QFeldspar.Expression.Utils.Reuse.GADTHigherOrder where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTHigherOrder
import QFeldspar.Environment.Typed
import QFeldspar.Conversion
import QFeldspar.Singleton
import qualified QFeldspar.Type.GADT as TFG
import qualified QFeldspar.Expression.GADTFirstOrder as GFO
import QFeldspar.Expression.Conversions.Lifting (cnvHOFOF,cnvFOHOF)

onHOAS :: forall g a.
          (HasSin TFG.Typ a , HasSin (Env TFG.Typ) g) =>
          (HasSin TFG.Typ a => GFO.Exp g a -> GFO.Exp g a) ->
          Exp g a -> Exp g a
onHOAS f e =  let g = sin :: Env TFG.Typ g
                  e' :: GFO.Exp g a = frmRgtZro (cnv (e , g))
              in frmRgtZro (cnv (f e' , g))

onHOASF :: forall g a b.
          (HasSin TFG.Typ a ,  HasSin TFG.Typ b ,
           HasSin (Env TFG.Typ) g) =>
          ((HasSin TFG.Typ a , HasSin TFG.Typ b) =>
           GFO.Exp (a ': g) b -> GFO.Exp (a ': g) b) ->
          (Exp g a -> Exp g b) -> Exp g a -> Exp g b
onHOASF f e =  let g = sin :: Env TFG.Typ g
                   e' :: GFO.Exp (a ': g) b = cnvHOFOF g e
               in cnvFOHOF g (f e')
