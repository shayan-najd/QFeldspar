module QFeldspar.Expression.Feldspar.Conversions.Lifting () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Expression.Feldspar.GADTFirstOrder  as FGFO
import qualified QFeldspar.Expression.Feldspar.GADTHigherOrder as FGHO

import qualified QFeldspar.Type.Feldspar.GADT                  as TFG

import QFeldspar.Variable.Typed    as VT
import QFeldspar.Environment.Typed as ET

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()

instance (t ~ t' , r ~ r') =>
         Cnv (FGFO.Exp r t , Env TFG.Typ r) (FGHO.Exp r' t') where
  cnv (e , r) = cnv (e , (ET.fmap FGHO.Var . cnvGEnvtoGVar) r)

cnvGEnvtoGVar ::  Env tf r -> Env (VT.Var r) r
cnvGEnvtoGVar ET.Emp        = ET.Emp
cnvGEnvtoGVar (ET.Ext _ xs) = ET.Ext VT.Zro (ET.fmap VT.Suc (cnvGEnvtoGVar xs))

instance (t ~ t' , r ~ r') =>
         Cnv (FGFO.Exp r t , Env (FGHO.Exp r) r) (FGHO.Exp r' t') where
  cnv (ee , r) = let ?r = r in case ee of
      FGFO.ConI i       -> pure (FGHO.ConI i)
      FGFO.ConB b       -> pure (FGHO.ConB b)
      FGFO.ConF f       -> pure (FGHO.ConF f)
      FGFO.Var v        -> id        <$@> v
      FGFO.Abs eb       -> FGHO.Abs  <$@> eb
      FGFO.App ef ea    -> FGHO.App  <$@> ef <*@> ea
      FGFO.Cnd ec et ef -> FGHO.Cnd  <$@> ec <*@> et <*@> ef
      FGFO.Whl ec eb ei -> FGHO.Whl  <$@> ec <*@> eb <*@> ei
      FGFO.Tpl ef es    -> FGHO.Tpl  <$@> ef <*@> es
      FGFO.Fst e        -> FGHO.Fst  <$@> e
      FGFO.Snd e        -> FGHO.Snd  <$@> e
      FGFO.Ary el ef    -> FGHO.Ary  <$@> el <*@> ef
      FGFO.Len e        -> FGHO.Len  <$@> e
      FGFO.Ind ea ei    -> FGHO.Ind  <$@> ea <*@> ei
      FGFO.AryV el ef   -> FGHO.AryV  <$@> el <*@> ef
      FGFO.LenV e       -> FGHO.LenV <$@> e
      FGFO.IndV ea ei   -> FGHO.IndV <$@> ea <*@> ei
      FGFO.Let el eb    -> FGHO.Let  <$@> el <*@> eb
      FGFO.Cmx er ei    -> FGHO.Cmx  <$@> er <*@> ei
      FGFO.Non          -> pure FGHO.Non
      FGFO.Som e        -> FGHO.Som  <$@> e
      FGFO.May em en es -> FGHO.May  <$@> em <*@> en <*@> es
      FGFO.Mul er ei    -> FGHO.Mul  <$@> er <*@> ei

instance (ta ~ ta' , tb ~ tb' , r ~ r') =>
         Cnv (FGFO.Exp (ta ': r) tb , Env (FGHO.Exp r) r)
             (FGHO.Exp r' ta' -> FGHO.Exp r' tb')
         where
  cnv (eb , r) = pure (FGHO.prdAll
                      . frmRgt . cnv' eb
                      . ET.fmap FGHO.sucAll
                      . flip Ext r)
    where
      cnv' :: forall rr tt. FGFO.Exp rr tt  -> Env (FGHO.Exp rr) rr ->
              ErrM (FGHO.Exp rr tt)
      cnv' = curry cnv
