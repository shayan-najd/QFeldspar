module QFeldspar.Expression.Conversions.TypeInference () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Expression.GADTTyped           as FGTD

import qualified QFeldspar.Type.Herbrand                           as TH
import qualified QFeldspar.Type.ADT                       as TFA

import QFeldspar.Environment.Scoped

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion   ()

import QFeldspar.Inference             (typInf)

instance (m ~ m' , n ~ n') =>
      Cnv (FGTD.Exp m n (Maybe TFA.Typ) , (Env m TFA.Typ , Env n TFA.Typ)) (FGTD.Exp m' n' TFA.Typ) where
  cnv (e , (s , g)) = do
    s' :: Env m (TH.Typ (TH.EnvFld '[])) <- cnv (s , ())
    g' :: Env n (TH.Typ (TH.EnvFld '[])) <- cnv (g, ())
    e'  <- traverse (maybe (return Nothing) (fmap Just . (\ t -> cnv (t , ()))))  e
    e'' <- typInf e' (s' , g')
    traverse (\ t -> cnv (t , ()))  e''
