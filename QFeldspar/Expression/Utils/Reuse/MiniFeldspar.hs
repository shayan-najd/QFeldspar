module QFeldspar.Expression.Utils.Reuse.MiniFeldspar where

import QFeldspar.MyPrelude
import QFeldspar.Expression.MiniFeldspar
import QFeldspar.Environment.Typed
import QFeldspar.Conversion
import QFeldspar.Singleton
import qualified QFeldspar.Type.GADT as TFG
import qualified QFeldspar.Expression.GADTFirstOrder as GFO
import qualified QFeldspar.Expression.GADTHigherOrder as GHO
import QFeldspar.Expression.Utils.Reuse.GADTHigherOrder
    (onHOAS,onHOASF)
import QFeldspar.Expression.Conversions.Normalisation ()

onMF :: forall g a.
          (HasSin TFG.Typ a , HasSin (Env TFG.Typ) g) =>
          (HasSin TFG.Typ a => GFO.Exp g a -> GFO.Exp g a) ->
          Exp g a -> Exp g a
onMF f e =  let e' :: GHO.Exp g a = frmRgtZro (cnv (e , ()))
                e'' = onHOAS f e'
            in frmRgtZro (cnv (e'' , ()))

onMFF :: forall g a b.
          (HasSin TFG.Typ a ,  HasSin TFG.Typ b ,
           HasSin (Env TFG.Typ) g) =>
          ((HasSin TFG.Typ a , HasSin TFG.Typ b) =>
           GFO.Exp (a ': g) b -> GFO.Exp (a ': g) b) ->
          (Exp g a -> Exp g b) -> Exp g a -> Exp g b
onMFF f e = let e' :: GHO.Exp g a -> GHO.Exp g b =
                      frmRgtZro (cnv (e , ()))
            in  frmRgtZro (cnv (onHOASF f e',()))
