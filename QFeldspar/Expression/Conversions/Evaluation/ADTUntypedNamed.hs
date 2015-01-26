{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.Evaluation.ADTUntypedNamed () where

import QFeldspar.MyPrelude

import QFeldspar.Expression.ADTUntypedNamed
import qualified QFeldspar.Expression.ADTValue as FAV

import QFeldspar.Environment.Map

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()

instance Eq v => Cnv (Exp v , Env v FAV.Exp) FAV.Exp where
  cnv (ee , r) = let ?r = r in join (case ee of
    Var  v       -> pure (get v r)
    AryV _ _     -> impossibleM
    LenV _       -> impossibleM
    IndV _ _     -> impossibleM
    Non          -> impossibleM
    Som _        -> impossibleM
    May _ _ _    -> impossibleM
    Mul _ _      -> impossibleM
    Let el eb    -> FAV.leT  <$@> el <*@> eb
    _ -> $(biGenOverloadedML 'ee ''Exp "FAV"
     ['Var,'AryV,'LenV,'IndV,'Non,'Som,'May,'Mul,'Let]
     (const [| cnvImp |])))

instance Eq v => Cnv ((v , Exp v) , Env v FAV.Exp) (FAV.Exp -> FAV.Exp) where
  cnv ((x , e) , r) = pure (frmRgtZro . curry cnv e . (: r) . (,) x)
