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
    AryV _ _                 -> impossibleM
    LenV _                   -> impossibleM
    IndV _ _                 -> impossibleM
    Non                      -> impossibleM
    Som _                    -> impossibleM
    May _ _ _                -> impossibleM
    Mul er ei                -> case t of
                     TFG.Int -> FGV.mul  <$@> er <*@> ei
                     TFG.Flt -> FGV.mul  <$@> er <*@> ei
                     _       -> fail "Type Error in Mul"
    Let el eb                -> FGV.leT <$@> el <*@> eb
    _  -> $(biRecAppMQS 'ee ''Exp "FGV" ['AryV,'LenV,'IndV,'Non,'Som,'May,'Mul,'Let]
            (trvWrp 't))

instance (ta' ~ ta , tb' ~ tb , HasSin TFG.Typ ta , HasSin TFG.Typ tb) =>
         Cnv (Exp (ta ': r) tb , Env FGV.Exp r)  (FGV.Exp (Arr ta' tb'))
         where
  cnv  (e , r) = (pure . FGV.Exp)
                  (FGV.getTrm . frmRgt . curry cnv e
                   . flip Ext r . (FGV.Exp :: ta -> FGV.Exp ta))
