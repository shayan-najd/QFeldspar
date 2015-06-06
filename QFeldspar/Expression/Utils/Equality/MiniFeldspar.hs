module QFeldspar.Expression.Utils.Equality.MiniFeldspar (eql,eqlF) where

import QFeldspar.MyPrelude

import QFeldspar.Conversion
import QFeldspar.Singleton
import QFeldspar.Environment.Typed
import QFeldspar.Expression.MiniFeldspar
import qualified QFeldspar.Type.GADT as TG
import qualified QFeldspar.Expression.GADTHigherOrder as GHO
import qualified QFeldspar.Expression.Utils.Equality.GADTHigherOrder as GHO
import QFeldspar.Expression.Conversion ()

eql :: forall a g.
       (HasSin TG.Typ a , HasSin (Env TG.Typ) g) =>
       Exp g a -> Exp g a -> Bool
eql e1 e2 = let e1' :: GHO.Exp g a = frmRgtZro (cnv (e1 , ()))
                e2' :: GHO.Exp g a = frmRgtZro (cnv (e2 , ()))
            in  GHO.eql e1' e2'

eqlF :: forall a b g.
        (HasSin TG.Typ a , HasSin TG.Typ b , HasSin (Env TG.Typ) g) =>
        (Exp g a -> Exp g b) -> (Exp g a -> Exp g b) -> Bool
eqlF f1 f2 = let f1' :: GHO.Exp g a -> GHO.Exp g b = frmRgtZro (cnv (f1 , ()))
                 f2' :: GHO.Exp g a -> GHO.Exp g b = frmRgtZro (cnv (f2 , ()))
             in GHO.eqlF f1' f2'
