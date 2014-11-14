module QFeldspar.Expression.Conversions.Evaluation.ADTUntypedNamed () where

import QFeldspar.MyPrelude

import QFeldspar.Expression.ADTUntypedNamed
import qualified QFeldspar.Expression.ADTValue as FAV

import QFeldspar.Environment.Map

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()

instance Eq v => Cnv (Exp v , Env v FAV.Exp) FAV.Exp where
  cnv (ee , r) = let ?r = r in join (case ee of
    ConI i             -> pure (FAV.conI i)
    ConB b             -> pure (FAV.conB b)
    ConF f             -> pure (FAV.conF f)
    Var x              -> FAV.var  <$@> x
    Abs xeb            -> FAV.abs  <$@> xeb
    App ef ea          -> FAV.app  <$@> ef  <*@> ea
    Cnd ec et ef       -> FAV.cnd  <$@> ec  <*@> et <*@> ef
    Whl xec xeb ei     -> FAV.whl  <$@> xec <*@> xeb <*@> ei
    Tpl ef es          -> FAV.tpl  <$@> ef  <*@> es
    Fst e              -> FAV.fst  <$@> e
    Snd e              -> FAV.snd  <$@> e
    Ary el xef         -> FAV.ary  <$@> el  <*@> xef
    Len e              -> FAV.len  <$@> e
    Ind ea ei          -> FAV.ind  <$@> ea <*@> ei
    Let el xeb         -> pure     <$@> App (Abs xeb) el
    Cmx er ei          -> FAV.cmx  <$@> er <*@> ei
    Typ _  e           -> pure (cnvImp e)
    Mul el er          -> FAV.mul  <$@> el <*@> er
    _                  -> impossibleM)

instance Eq v => Cnv ((v , Exp v) , Env v FAV.Exp) (FAV.Exp -> FAV.Exp) where
  cnv ((x , e) , r) = pure (frmRgt . curry cnv e . (: r) . (,) x)
