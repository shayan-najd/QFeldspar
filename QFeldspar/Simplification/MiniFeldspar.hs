module QFeldspar.Simplification.MiniFeldspar (smp,smpF) where

import QFeldspar.MyPrelude

import QFeldspar.Conversion
import QFeldspar.Singleton
import QFeldspar.Environment.Typed
import QFeldspar.Expression.MiniFeldspar
import qualified QFeldspar.Type.GADT as TFG
import qualified QFeldspar.Expression.GADTHigherOrder as GHO
import qualified QFeldspar.Simplification.GADTHigherOrder as GHO
import QFeldspar.Expression.Conversion ()
import QFeldspar.Expression.Conversions.Normalisation ()

smp :: forall a g.
       (HasSin TFG.Typ a , HasSin (Env TFG.Typ) g) =>
       Exp g a -> Exp g a
smp e = let g = sin :: Env TFG.Typ g
            e' :: GHO.Exp g a = frmRgtZro (cnv (e , g))
            e'' = GHO.smp e'
         in frmRgtZro (cnv (e'' , g))

smpF :: forall a b g.
        (HasSin TFG.Typ a , HasSin TFG.Typ b , HasSin (Env TFG.Typ) g) =>
        (Exp g a -> Exp g b) -> (Exp g a -> Exp g b)
smpF f = let f' :: GHO.Exp g a -> GHO.Exp g b = frmRgtZro (cnv (f , ()))
             f'' = GHO.smpF f'
         in  frmRgtZro (cnv (f'',()))
