{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.Evaluation.GADTFirstOrder () where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTFirstOrder
import qualified QFeldspar.Expression.GADTValue as FGV
import qualified QFeldspar.Type.GADT as TFG
import QFeldspar.Environment.Typed
import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()
import QFeldspar.Singleton
import QFeldspar.Expression.Utils.Common

instance (HasSin TFG.Typ t , t' ~ t) =>
         Cnv (Exp r t , Env FGV.Exp r) (FGV.Exp t') where
  cnv (ee , r) = let ?r = r in let t = sin :: TFG.Typ t in case ee of
    Var  v                   -> pure (get v r)
    AryV _ _                 -> impossibleM
    LenV _                   -> impossibleM
    IndV _ _                 -> impossibleM
    Non                      -> impossibleM
    Som _                    -> impossibleM
    May _ _ _                -> impossibleM
    Mul er ei                -> case t of
      TFG.Int                -> FGV.mul  <$@> er <*@> ei
      TFG.Flt                -> FGV.mul  <$@> er <*@> ei
      _                      -> fail "Type Error in Mul"
    Add er ei                -> case t of
      TFG.Int                -> FGV.add  <$@> er <*@> ei
      TFG.Flt                -> FGV.add  <$@> er <*@> ei
      _                      -> fail "Type Error in Add"
    Int i                    -> case t of
      TFG.Int                -> pure (FGV.conI i)
      TFG.Flt                -> pure (FGV.conF (fromIntegral i))
      _                      -> fail "Type Error in Int"
    Let el eb                -> FGV.leT <$@> el <*@> eb
    Tag s e                  -> FGV.tag s <$@> e
    _  -> $(biGenOverloadedMWL 'ee ''Exp "FGV"
     ['Var,'AryV,'LenV,'IndV,'Non,'Som,'May,'Mul,'Add,'Let,'Int,'Tag]
     (trvWrp 't) (const [| cnvImp |]))

instance (ta' ~ ta , tb' ~ tb , HasSin TFG.Typ ta , HasSin TFG.Typ tb) =>
         Cnv (Exp (ta ': r) tb , Env FGV.Exp r)  (FGV.Exp (Arr ta' tb'))
         where
  cnv  (e , r) = (pure . FGV.Exp)
                  (FGV.getTrm . frmRgtZro . curry cnv e
                   . flip Ext r . (FGV.Exp :: ta -> FGV.Exp ta))
