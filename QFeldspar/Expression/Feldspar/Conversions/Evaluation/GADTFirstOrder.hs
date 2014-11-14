module QFeldspar.Expression.Feldspar.Conversions.Evaluation.GADTFirstOrder () where

import QFeldspar.MyPrelude

import QFeldspar.Expression.Feldspar.GADTFirstOrder
import qualified QFeldspar.Expression.Feldspar.GADTValue as FGV

import qualified QFeldspar.Type.Feldspar.GADT as TFG

import QFeldspar.Environment.Typed

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()

import QFeldspar.Singleton

instance (HasSin TFG.Typ t , t' ~ t) =>
         Cnv (Exp r t , Env FGV.Exp r) (FGV.Exp t') where
  cnv (ee , r) = let ?r = r in let t = sin :: TFG.Typ t in case ee of
    ConI i                   -> pure (FGV.conI i)
    ConB b                   -> pure (FGV.conB b)
    ConF f                   -> pure (FGV.conF f)
    Var x                    -> FGV.var  <$@> x
    Abs eb                   -> case TFG.getPrfHasSinArr t of
     (PrfHasSin , PrfHasSin) -> cnvImp eb
    App ef ea                -> FGV.app  <$@> ef <*@> ea
    Cnd ec et ef             -> FGV.cnd  <$@> ec <*@> et <*@> ef
    Whl ec eb ei             -> FGV.whl  <$@> ec <*@> eb <*@> ei
    Tpl ef es                -> case TFG.getPrfHasSinTpl t of
     (PrfHasSin , PrfHasSin) -> FGV.tpl  <$@> ef <*@> es
    Fst e                    -> FGV.fst  <$@> e
    Snd e                    -> FGV.snd  <$@> e
    Ary el ef                -> case TFG.getPrfHasSinAry t of
     PrfHasSin               -> FGV.ary  <$@> el <*@> ef
    Len e                    -> FGV.len  <$@> e
    Ind ea ei                -> FGV.ind  <$@> ea <*@> ei
    Let el eb                -> cnvImp (App (Abs eb) el)
    Cmx er ei                -> FGV.cmx  <$@> er <*@> ei
    Non                      -> pure FGV.non
    Som e                    -> case TFG.getPrfHasSinMay t of
     PrfHasSin               -> FGV.som  <$@> e
    May em en es             -> FGV.may  <$@> em <*@> en <*@> es
    _                        -> impossibleM

instance (ta' ~ ta , tb' ~ tb , HasSin TFG.Typ ta , HasSin TFG.Typ tb) =>
         Cnv (Exp (ta ': r) tb , Env FGV.Exp r)  (FGV.Exp (Arr ta' tb'))
         where
  cnv  (e , r) = (pure . FGV.Exp)
                  (FGV.getTrm . frmRgt . curry cnv e
                   . flip Ext r . (FGV.Exp :: ta -> FGV.Exp ta))
