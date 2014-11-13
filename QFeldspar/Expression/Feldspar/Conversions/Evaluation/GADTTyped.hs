module QFeldspar.Expression.Feldspar.Conversions.Evaluation.GADTTyped () where

import QFeldspar.MyPrelude

import QFeldspar.Expression.Feldspar.GADTTyped
import qualified QFeldspar.Expression.Feldspar.ADTValue as FAV

import QFeldspar.Environment.Scoped

import QFeldspar.Nat.ADT

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()

instance Cnv (Exp n t , Env n FAV.Exp) FAV.Exp where
  cnv (ee , r) = let ?r = r in join (case ee of
    ConI i       -> pure (FAV.conI i)
    ConB b       -> pure (FAV.conB b)
    ConF f       -> pure (FAV.conF f)
    Var x        -> FAV.var  <$@> x
    Abs eb       -> FAV.abs  <$@> eb
    App _  ef ea -> FAV.app  <$@> ef <*@> ea
    Cnd ec et ef -> FAV.cnd  <$@> ec <*@> et <*@> ef
    Whl ec eb ei -> FAV.whl  <$@> ec <*@> eb <*@> ei
    Tpl ef es    -> FAV.tpl  <$@> ef <*@> es
    Fst _  e     -> FAV.fst  <$@> e
    Snd _  e     -> FAV.snd  <$@> e
    Ary el ef    -> FAV.ary  <$@> el <*@> ef
    Len _  e     -> FAV.len  <$@> e
    Ind ea ei    -> FAV.ind  <$@> ea <*@> ei
    Let _  el eb -> return   <$@> App impossible (Abs eb) el
    Cmx er ei    -> FAV.cmx  <$@> er <*@> ei
    Typ _ e      -> pure (cnvImp e)
    _            -> impossibleM)

instance Cnv (Exp (Suc n) t , Env n FAV.Exp) (FAV.Exp -> FAV.Exp) where
  cnv (e , r) = pure (frmRgt . curry cnv e . (flip Ext r))
