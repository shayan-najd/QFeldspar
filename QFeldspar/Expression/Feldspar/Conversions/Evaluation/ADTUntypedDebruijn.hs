module QFeldspar.Expression.Feldspar.Conversions.Evaluation.ADTUntypedDebruijn () where

import QFeldspar.MyPrelude

import QFeldspar.Expression.Feldspar.ADTUntypedDebruijn
import qualified QFeldspar.Expression.Feldspar.ADTValue as FAV

import QFeldspar.Environment.Plain

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()

instance Cnv (Exp , Env FAV.Exp) FAV.Exp where
  cnv (ee , r) = let ?r = r in join (case ee of
    ConI i       -> FAV.conI <$@> i
    ConB b       -> FAV.conB <$@> b
    ConF b       -> FAV.conF <$@> b
    Var x        -> FAV.var  <$@> x
    Abs eb       -> FAV.abs  <$@> eb
    App ef ea    -> FAV.app  <$@> ef <*@> ea
    Cnd ec et ef -> FAV.cnd  <$@> ec <*@> et <*@> ef
    Whl ec eb ei -> FAV.whl  <$@> ec <*@> eb <*@> ei
    Tpl ef es    -> FAV.tpl  <$@> ef <*@> es
    Fst e        -> FAV.fst  <$@> e
    Snd e        -> FAV.snd  <$@> e
    Ary el ef    -> FAV.ary  <$@> el <*@> ef
    Len e        -> FAV.len  <$@> e
    Ind ea ei    -> FAV.ind  <$@> ea <*@> ei
    Let el eb    -> pure     <$@> App (Abs eb) el
    Cmx er ei    -> FAV.cmx  <$@> er <*@> ei
    Non          -> pure FAV.non
    Som e        -> FAV.som  <$@> e
    May em en es -> FAV.may  <$@> em <*@> en <*@> es)

instance Cnv (Exp , Env FAV.Exp)  (FAV.Exp -> FAV.Exp) where
  cnv (e , r) = pure (frmRgt . curry cnv e . (: r))