module QFeldspar.Eta where

import QFeldspar.MyPrelude

import QFeldspar.Expression.Feldspar.GADTHigherOrder
import QFeldspar.Singleton
import qualified QFeldspar.Type.Feldspar.GADT as TFG

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
    ConI i                    -> ConI i
    ConB b                    -> ConB b
    ConF f                    -> ConF f
    Var x                     -> Var  x
    Abs eb                    -> case TFG.getPrfHasSinArr t of
      (PrfHasSin , PrfHasSin) -> Abs (etaF eb)
    App ef             ea     -> App (etasub ef) (eta ea)
    Cnd ec et ef              -> Cnd (eta ec) (eta et) (eta ef)
    Whl ec eb ei              -> Whl (eta ec) (eta eb) (eta ei)
    Tpl ef es                 -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin) -> Tpl (eta ef) (eta es)
    Fst e                     -> Fst (eta e)
    Snd e                     -> Snd (eta e)
    Ary el ef                 -> case TFG.getPrfHasSinAry t of
      PrfHasSin               -> Ary (eta el) (eta ef)
    Len e                     -> Len (eta e)
    Ind ea             ei     -> Ind (eta ea) (eta ei)
    AryV el ef                 -> case TFG.getPrfHasSinVec t of
      PrfHasSin               -> AryV (eta el) (eta ef)
    LenV e                     -> LenV (eta e)
    IndV ea             ei     -> IndV (eta ea) (eta ei)
    Let el eb                 -> Let (eta el) (etaF eb)
    Cmx er ei                 -> Cmx (eta er) (eta ei)
    Tmp x                     -> Tmp x
    Non                       -> Non
    Som e                     -> case TFG.getPrfHasSinMay t of
      PrfHasSin               -> Som (eta e)
    May em en es              -> May (eta em) (eta en) (eta es)
    Mul er ei                 -> Mul (eta er) (eta ei)

etaF :: forall n ta tb. (HasSin TFG.Typ ta , HasSin TFG.Typ tb) =>
           (Exp n ta -> Exp n tb) -> (Exp n ta -> Exp n tb)
etaF f = let v  = genNewNam "__etaF__"
             {-# NOINLINE v #-}
         in deepseq v $ (\ x -> absTmp x v (eta (f (Tmp v))))
