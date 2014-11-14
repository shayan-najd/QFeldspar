module QFeldspar.Expression.Conversions.TypeInference () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Expression.GADTTyped           as FGTD

import qualified QFeldspar.Type.Herbrand                           as TH
import qualified QFeldspar.Type.ADT                       as TFA

import QFeldspar.Environment.Scoped

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion   ()

import QFeldspar.Inference             (typInf)

instance n ~ n' =>
      Cnv (FGTD.Exp n (Maybe TFA.Typ) , Env n TFA.Typ)(FGTD.Exp n' TFA.Typ) where
  cnv (e , r) = let ?r = r in
    do r' :: Env n (TH.Typ (TH.EnvFld '[])) <- cnvImp r
       e'  <- traverse (maybe (return Nothing) (fmap Just . cnvImp))  e
       e'' <- typInf e' r'
       traverse cnvImp e''
