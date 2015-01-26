{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.Evaluation.GADTTyped () where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTTyped
import qualified QFeldspar.Expression.ADTValue as FAV
import QFeldspar.Environment.Scoped
import QFeldspar.Nat.ADT
import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()

instance Cnv (Exp n t , Env n FAV.Exp) FAV.Exp where
  cnv (ee , r) = let ?r = r in join (case ee of
    Var v        -> pure (pure (get v r))
    App _  ef ea -> FAV.app  <$@> ef <*@> ea
    Fst _  e     -> FAV.fst  <$@> e
    Snd _  e     -> FAV.snd  <$@> e
    Len _  e     -> FAV.len  <$@> e
    Let _  el eb -> FAV.leT  <$@> el <*@> eb
    Typ _ e      -> pure (cnvImp e)
    AryV _ _     -> impossibleM
    LenV _ _     -> impossibleM
    IndV _ _     -> impossibleM
    Non          -> impossibleM
    Som _        -> impossibleM
    May _ _ _ _  -> impossibleM
    Mul _ _      -> impossibleM
    _ -> $(biGenOverloadedML 'ee ''Exp "FAV"
     ['Var,'App,'Fst,'Snd,'Len,'Let,'Typ,'AryV,'LenV,'IndV,
      'Non,'Som,'May,'Mul] (const [| cnvImp |])))

instance Cnv (Exp (Suc n) t , Env n FAV.Exp) (FAV.Exp -> FAV.Exp) where
  cnv (e , r) = pure (frmRgtZro . curry cnv e . (flip Ext r))
