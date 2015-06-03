{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.Evaluation.GADTTyped () where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTTyped
import qualified QFeldspar.Expression.ADTValue as FAV
import QFeldspar.Environment.Scoped
import QFeldspar.Nat.ADT
import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()

instance Cnv (Exp m n t , (Env m FAV.Exp , Env n FAV.Exp)) FAV.Exp where
  cnv (ee , r@(s , g)) = let ?r = r in join (case ee of
    Var v        -> pure (pure (get v g))
    App _  ef ea -> FAV.app  <$@> ef <*@> ea
    Fst _  e     -> FAV.fst  <$@> e
    Snd _  e     -> FAV.snd  <$@> e
    Len _  e     -> FAV.len  <$@> e
    Let _  el eb -> FAV.leT  <$@> el <*@> eb
    Eql _  el eb -> FAV.eql  <$@> el <*@> eb
    Ltd _  el eb -> FAV.ltd  <$@> el <*@> eb
    Typ _ e      -> pure (cnvImp e)
    AryV _ _     -> impossibleM
    LenV _ _     -> impossibleM
    IndV _ _     -> impossibleM
    Non          -> impossibleM
    Som _        -> impossibleM
    May _ _ _ _  -> impossibleM
    Prm _ v es   -> FAV.prm (get v s) <$> mapM cnvImp  es
    _ -> $(biGenOverloadedML 'ee ''Exp "FAV"
     ['Prm,'Var,'App,'Fst,'Snd,'Len,'Let,'Typ,'AryV,'LenV,'IndV,'Eql,'Ltd,
      'Non,'Som,'May] (const [| cnvImp |])))

instance Cnv (Exp m (Suc n) a , (Env m FAV.Exp , Env n FAV.Exp)) (FAV.Exp -> FAV.Exp) where
  cnv (e , (s , g)) = pure (\ x -> frmRgtZro (cnv (e , (s , Ext x g))))
