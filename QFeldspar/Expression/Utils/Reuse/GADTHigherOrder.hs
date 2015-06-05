module QFeldspar.Expression.Utils.Reuse.GADTHigherOrder where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTHigherOrder
import QFeldspar.Environment.Typed
import QFeldspar.Conversion
import QFeldspar.Singleton
import qualified QFeldspar.Type.GADT as TFG
import qualified QFeldspar.Expression.GADTFirstOrder as GFO
import QFeldspar.Expression.Conversions.Lifting (cnvHOFOF,cnvFOHOF)

onHOAS :: forall s a.
          (TFG.Type a , TFG.Types s) =>
          (TFG.Type a => GFO.Exp s '[] a -> GFO.Exp s '[] a) ->
          Exp s a -> Exp s a
onHOAS f e =  let g = sin :: Env TFG.Typ s
                  e' :: GFO.Exp s '[] a = frmRgtZro (cnv (e , g))
              in frmRgtZro (cnv (f e' , g))

onHOASF :: forall s a b.
          (TFG.Type a , TFG.Type b , TFG.Types s) =>
          ((TFG.Type a , TFG.Type b) =>
           GFO.Exp s '[a] b -> GFO.Exp s '[a] b) ->
          (Exp s a -> Exp s b) -> Exp s a -> Exp s b
onHOASF f e =  let g = sin :: Env TFG.Typ s
                   e' :: GFO.Exp s '[a] b = cnvHOFOF g e
               in cnvFOHOF (f e')
