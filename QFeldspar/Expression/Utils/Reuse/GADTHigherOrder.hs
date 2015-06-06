module QFeldspar.Expression.Utils.Reuse.GADTHigherOrder where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTHigherOrder
import QFeldspar.Environment.Typed
import QFeldspar.Conversion
import QFeldspar.Singleton
import qualified QFeldspar.Type.GADT as TG
import qualified QFeldspar.Expression.GADTFirstOrder as GFO
import QFeldspar.Expression.Conversions.Lifting (cnvHOFOF,cnvFOHOF)

onHOAS :: forall s a.
          (TG.Type a , TG.Types s) =>
          (TG.Type a => GFO.Exp s '[] a -> GFO.Exp s '[] a) ->
          Exp s a -> Exp s a
onHOAS f e =  let g = sin :: Env TG.Typ s
                  e' :: GFO.Exp s '[] a = frmRgtZro (cnv (e , g))
              in frmRgtZro (cnv (f e' , g))

onHOASF :: forall s a b.
          (TG.Type a , TG.Type b , TG.Types s) =>
          ((TG.Type a , TG.Type b) =>
           GFO.Exp s '[a] b -> GFO.Exp s '[a] b) ->
          (Exp s a -> Exp s b) -> Exp s a -> Exp s b
onHOASF f e =  let g = sin :: Env TG.Typ s
                   e' :: GFO.Exp s '[a] b = cnvHOFOF g e
               in cnvFOHOF (f e')
