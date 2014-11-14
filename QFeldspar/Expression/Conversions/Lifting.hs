{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.Lifting () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Expression.GADTFirstOrder  as FGFO
import qualified QFeldspar.Expression.GADTHigherOrder as FGHO
import qualified QFeldspar.Expression.Utils.GADTHigherOrder as FGHO (sucAll,prdAll)
import qualified QFeldspar.Type.GADT                  as TFG

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
	FGFO.Var v -> id        <$@> v
        _          -> $(biRecAppMQW 'ee ''FGFO.Exp "FGHO" ['FGFO.Var]
                                        (const id))

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
