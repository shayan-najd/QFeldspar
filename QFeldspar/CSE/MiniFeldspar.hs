-- {-# LANGUAGE UndecidableInstances #-}
module QFeldspar.CSE.MiniFeldspar (cse,cseF) where

import QFeldspar.MyPrelude

import QFeldspar.Conversion
import QFeldspar.Singleton
import QFeldspar.Environment.Typed
import QFeldspar.Expression.MiniFeldspar
import qualified QFeldspar.Type.GADT as TFG
import qualified QFeldspar.Expression.GADTHigherOrder as GHO
import qualified QFeldspar.CSE.GADTHigherOrder as GHO
import QFeldspar.Expression.Conversions.Normalisation ()

cse :: forall a g.
       (HasSin TFG.Typ a , HasSin (Env TFG.Typ) g) =>
       Exp g a -> Exp g a
cse e = let e' :: GHO.Exp g a = frmRgtZro (cnv (e , ()))
            e'' = GHO.cse e'
         in frmRgtZro (cnv (e'' , ()))

cseF :: forall a b g.
        (HasSin TFG.Typ a , HasSin TFG.Typ b , HasSin (Env TFG.Typ) g) =>
        (Exp g a -> Exp g b) -> (Exp g a -> Exp g b)
cseF f = let f' :: GHO.Exp g a -> GHO.Exp g b = frmRgtZro (cnv (f , ()))
             f'' = GHO.cseF f'
         in  frmRgtZro (cnv (f'',()))
