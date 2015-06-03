{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.Evaluation.ADTUntypedNamed () where

import QFeldspar.MyPrelude

import QFeldspar.Expression.ADTUntypedNamed
import qualified QFeldspar.Expression.ADTValue as FAV

import QFeldspar.Environment.Map

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()

instance (Show v , Eq v) => Cnv (Exp v , (Env v FAV.Exp , Env v FAV.Exp)) FAV.Exp where
  cnv (ee , r@(s , g)) = let ?r = r in join (case ee of
    Var  v       -> pure (get v g)
    AryV _ _     -> impossibleM
    LenV _       -> impossibleM
    IndV _ _     -> impossibleM
    Non          -> impossibleM
    Som _        -> impossibleM
    May _ _ _    -> impossibleM
    Let el eb    -> FAV.leT  <$@> el <*@> eb
    Prm v es     -> FAV.prm  <$> get v s <*> mapM cnvImp es
    _ -> $(biGenOverloadedML 'ee ''Exp "FAV"
     ['Prm,'Var,'AryV,'LenV,'IndV,'Non,'Som,'May,'Let]
     (const [| cnvImp |])))

instance (Show v , Eq v) => Cnv ((v , Exp v) , (Env v FAV.Exp , Env v FAV.Exp)) (FAV.Exp -> FAV.Exp) where
  cnv ((x , e) , (s , g)) = pure (\ v -> frmRgtZro (cnv (e  , (s , (x,v) : g))))
