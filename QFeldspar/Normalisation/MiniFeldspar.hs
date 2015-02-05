-- {-# LANGUAGE UndecidableInstances #-}
module QFeldspar.Normalisation.MiniFeldspar (nrm,nrmF) where

import QFeldspar.MyPrelude

import QFeldspar.Conversion
import QFeldspar.Singleton
import QFeldspar.Environment.Typed
import QFeldspar.Expression.MiniFeldspar
import qualified QFeldspar.Type.GADT as TFG
import qualified QFeldspar.Expression.GADTHigherOrder as GHO
import qualified QFeldspar.Normalisation.GADTHigherOrder as GHO
import QFeldspar.Expression.Conversion ()
import QFeldspar.Expression.Conversions.Normalisation ()

nrm :: forall a g.
       (HasSin TFG.Typ a , HasSin (Env TFG.Typ) g) =>
       Exp g a -> Exp g a
nrm e = let g = sin :: Env TFG.Typ g
            e' :: GHO.Exp g a = frmRgtZro (cnv (e , g))
            e'' = GHO.nrm e'
         in frmRgtZro (cnv (e'' , g))

nrmF :: forall a b g.
        (HasSin TFG.Typ a , HasSin TFG.Typ b , HasSin (Env TFG.Typ) g) =>
        (Exp g a -> Exp g b) -> (Exp g a -> Exp g b)
nrmF f = let f' :: GHO.Exp g a -> GHO.Exp g b = frmRgtZro (cnv (f , ()))
             f'' = GHO.nrmF f'
         in  frmRgtZro (cnv (f'',()))
