module QFeldspar.Expression.Conversions.TypeInference () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Expression.GADTTyped           as GTD

import qualified QFeldspar.Type.Herbrand                           as TH
import qualified QFeldspar.Type.ADT                       as TA

import QFeldspar.Environment.Scoped

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion   ()

import QFeldspar.Inference             (typInf)

instance (m ~ m' , n ~ n') =>
      Cnv (GTD.Exp m n (Maybe TA.Typ) , (Env m TA.Typ , Env n TA.Typ)) (GTD.Exp m' n' TA.Typ) where
  cnv (e , (s , g)) = do
    s' :: Env m (TH.Typ (TH.EnvFld '[])) <- cnv (s , ())
    g' :: Env n (TH.Typ (TH.EnvFld '[])) <- cnv (g, ())
    e'  <- traverse (maybe (return Nothing) (fmap Just . (\ t -> cnv (t , ()))))  e
    e'' <- typInf e' (s' , g')
    traverse (\ t -> cnv (t , ()))  e''
