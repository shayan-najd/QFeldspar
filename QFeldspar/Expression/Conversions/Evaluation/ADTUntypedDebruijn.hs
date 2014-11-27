{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.Evaluation.ADTUntypedDebruijn
       () where

import QFeldspar.MyPrelude

import QFeldspar.Expression.ADTUntypedDebruijn
import qualified QFeldspar.Expression.ADTValue as FAV

import QFeldspar.Environment.Plain

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()

instance Cnv (Exp , Env FAV.Exp) FAV.Exp where
  cnv (ee , r) = let ?r = r in join (case ee of
    AryV _ _     -> impossibleM
    LenV _       -> impossibleM
    IndV _ _     -> impossibleM
    Non          -> impossibleM
    Som _        -> impossibleM
    May _ _ _    -> impossibleM
    Mul _ _      -> impossibleM
    Let el eb    -> FAV.leT  <$@> el <*@> eb
    _ -> $(biRecAppMQS 'ee ''Exp "FAV"
     ['AryV,'LenV,'IndV,'Non,'Som,'May,'Mul,'Let]
     (const id)))

instance Cnv (Fun , Env FAV.Exp)  (FAV.Exp -> FAV.Exp) where
  cnv (Fun e , r) = pure (frmRgtZro . curry cnv e . (: r))
