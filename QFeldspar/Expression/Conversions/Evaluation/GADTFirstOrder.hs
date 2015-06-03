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

instance (HasSin TFG.Typ a', a ~ a') =>
         Cnv (Exp s g a , (Env FGV.Exp s, Env FGV.Exp g)) (FGV.Exp a') where
  cnv (ee , r@(s , g)) = let ?r = r in let t = sin :: TFG.Typ a in case ee of
    Var  v                   -> pure (get v g)
    AryV _ _                 -> impossibleM
    LenV _                   -> impossibleM
    IndV _ _                 -> impossibleM
    Non                      -> impossibleM
    Som _                    -> impossibleM
    May _ _ _                -> impossibleM
    Mul er ei                -> case t of
      TFG.Wrd                -> FGV.mul  <$@> er <*@> ei
      TFG.Flt                -> FGV.mul  <$@> er <*@> ei
      TFG.Cmx                -> FGV.mul  <$@> er <*@> ei
      _                      -> fail "Type Error in Mul"
    Add er ei                -> case t of
      TFG.Wrd                -> FGV.add  <$@> er <*@> ei
      TFG.Flt                -> FGV.add  <$@> er <*@> ei
      TFG.Cmx                -> FGV.add  <$@> er <*@> ei
      _                      -> fail "Type Error in Add"
    Sub er ei                -> case t of
      TFG.Wrd                -> FGV.sub  <$@> er <*@> ei
      TFG.Flt                -> FGV.sub  <$@> er <*@> ei
      TFG.Cmx                -> FGV.sub  <$@> er <*@> ei
      _                      -> fail "Type Error in Sub"
    Eql er ei                -> case sinTyp er of
      TFG.Wrd                -> FGV.eql  <$@> er <*@> ei
      TFG.Flt                -> FGV.eql  <$@> er <*@> ei
      TFG.Bol                -> FGV.eql  <$@> er <*@> ei
      _                      -> fail "Type Error in Eql"
    Ltd er ei                -> case sinTyp er of
      TFG.Wrd                -> FGV.ltd  <$@> er <*@> ei
      TFG.Flt                -> FGV.ltd  <$@> er <*@> ei
      TFG.Bol                -> FGV.ltd  <$@> er <*@> ei
      _                      -> fail "Type Error in Ltd"
    Int i                    -> case t of
      TFG.Wrd                -> pure (FGV.conI i)
      TFG.Flt                -> pure (FGV.conF (fromIntegral i))
      _                      -> fail "Type Error in Int"
    Let el eb                -> FGV.leT <$@> el <*@> eb
    Tag x e                  -> FGV.tag x <$@> e
    Prm v es                 -> FGV.prm (get v s) <$> TFG.mapMC (sinTyp v) cnvImp es
    _  -> $(biGenOverloadedMWL 'ee ''Exp "FGV"
     ['Var,'AryV,'LenV,'IndV,'Non,'Som,'May,'Mul,'Add,'Sub,'Eql,'Ltd,'Let,'Int,'Tag,'Prm]
     (trvWrp 't) (const [| cnvImp |]))

instance (a ~ a' , b ~ b' , HasSin TFG.Typ a' , HasSin TFG.Typ b') =>
         Cnv (Exp s (a ': g) b , (Env FGV.Exp s , Env FGV.Exp g))  (FGV.Exp (a' -> b'))
         where
  cnv  (e , (s , g)) = (pure . FGV.Exp)
                  (\ v -> FGV.getTrm (frmRgtZro (cnv (e , (s , Ext (FGV.Exp v :: FGV.Exp a) g)))))
