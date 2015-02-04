{-# LANGUAGE UndecidableInstances #-}
module QFeldspar.Normalisation.GADTHigherOrder () where

import QFeldspar.MyPrelude

import QFeldspar.Expression.GADTHigherOrder
import QFeldspar.Conversion
import QFeldspar.Environment.Typed
import QFeldspar.Normalisation
import QFeldspar.Singleton
import qualified QFeldspar.Type.GADT as TFG
import qualified QFeldspar.Expression.GADTFirstOrder as GFO
import QFeldspar.Normalisation.GADTFirstOrder ()
import QFeldspar.Expression.Conversions.Lifting ()

instance (HasSin TFG.Typ t, HasSin (Env TFG.Typ) g) => NrmOne (Exp g t) where
  nrmOne e = let g = sin :: Env TFG.Typ g
                 e' :: GFO.Exp g t = frmRgtZro (cnv (e , g))
             in do e'' <- nrmOne e'
                   return (frmRgtZro (cnv (e'' , g)))
