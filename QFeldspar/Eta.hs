{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Eta
       (eta) where

import QFeldspar.MyPrelude

import QFeldspar.Expression.GADTHigherOrder
import QFeldspar.Expression.Utils.GADTHigherOrder(absTmp)
import QFeldspar.Expression.Utils.Common
import QFeldspar.Singleton
import qualified QFeldspar.Type.GADT as TFG

eta :: forall n t. HasSin TFG.Typ t =>
       Exp n t -> Exp n t
eta e = let t = sin :: TFG.Typ t in
  case e of
    Abs eb -> case TFG.getPrfHasSinArr t of
        (PrfHasSin , PrfHasSin) -> Abs (etaF eb)
    _      -> case t of
      TFG.Arr _ _               -> case TFG.getPrfHasSinArr t of
        (PrfHasSin , PrfHasSin) -> Abs (\ x -> eta (App e x))
      _                         -> etasub e

etasub :: forall n t. HasSin TFG.Typ t =>
          Exp n t -> Exp n t
etasub ee = let t = sin :: TFG.Typ t in case ee of
  App ef ea -> App (etasub ef) (eta ea)
  _         -> $(genOverloadedW 'ee ''Exp  ['App] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| Exp t t -> Exp t t |] -> [| etaF |]
    | matchQ tt [t| Exp t t |]            -> [| eta |]
    | otherwise                           -> [| id |]))

etaF :: forall n ta tb. (HasSin TFG.Typ ta , HasSin TFG.Typ tb) =>
           (Exp n ta -> Exp n tb) -> (Exp n ta -> Exp n tb)
etaF f = let v  = genNewNam "__etaF__"
             {-# NOINLINE v #-}
         in deepseq v $ (\ x -> absTmp x v (eta (f (Tmp v))))
