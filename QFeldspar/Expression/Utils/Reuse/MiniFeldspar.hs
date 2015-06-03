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

onMF :: forall s a.
          (HasSin TFG.Typ a , HasSin (Env TFG.Typ) s) =>
          (HasSin TFG.Typ a => GFO.Exp s '[] a -> GFO.Exp s '[] a) ->
          Exp s a -> Exp s a
onMF f e =  let e' :: GHO.Exp s a = frmRgtZro (cnv (e , ()))
                e'' = onHOAS f e'
            in frmRgtZro (cnv (e'' , ()))

onMFF :: forall s a b.
          (HasSin TFG.Typ a ,  HasSin TFG.Typ b ,
           HasSin (Env TFG.Typ) s) =>
          ((HasSin TFG.Typ a , HasSin TFG.Typ b) =>
           GFO.Exp s '[a] b -> GFO.Exp s '[a] b) ->
          (Exp s a -> Exp s b) -> Exp s a -> Exp s b
onMFF f e = let e' :: GHO.Exp s a -> GHO.Exp s b =
                      frmRgtZro (cnv (e , ()))
            in  frmRgtZro (cnv (onHOASF f e',()))
