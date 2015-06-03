{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.Evaluation.GADTHigherOrder () where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTHigherOrder
import qualified QFeldspar.Expression.GADTValue as FGV
import qualified QFeldspar.Type.GADT as TFG
import QFeldspar.Environment.Typed hiding (fmap)
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
    Tmp _                    -> impossibleM
    Let el eb                -> FGV.leT <$@> el <*@> eb
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
    Tag s e                  -> FGV.tag s <$@> e
    Prm v es                 -> FGV.prm  (get v r) <$> TFG.mapMC (sinTyp v) cnvImp  es
    _  -> $(biGenOverloadedMWL 'ee ''Exp "FGV"
            ['Prm,'AryV,'LenV,'IndV,'Non,'Som,'May,'Mul,'Add,'Sub,'Eql,'Ltd,'Let,'Tmp,'Int,'Tag]
            (trvWrp 't) (const [| cnvImp |]))

instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb
         , ta' ~ ta , tb' ~ tb) =>
         Cnv (Exp r ta -> Exp r tb , Env FGV.Exp r)
             (FGV.Exp (ta' -> tb')) where
  cnv (f , r)  =  let ?r = r in
    pure (FGV.Exp ( FGV.getTrm
                  . frmRgtZro . cnvImp
                  . f
                  . frmRgtZro . cnvImp
                  . FGV.Exp ))

instance (HasSin TFG.Typ t , r ~ r' , t ~ t') =>
         Cnv (FGV.Exp t' , Env FGV.Exp r') (Exp r t)
         where
  cnv (FGV.Exp v , r) = let ?r = r in let t = sin :: TFG.Typ t in case t of
    TFG.Wrd                   -> pure (ConI v)
    TFG.Bol                   -> pure (ConB v)
    TFG.Flt                   -> pure (ConF v)
    TFG.Arr _ _               -> case TFG.getPrfHasSinArr t of
     (PrfHasSin , PrfHasSin)  -> Abs  <$@> samTyp t (FGV.Exp v)
    TFG.Tpl _ _               -> case TFG.getPrfHasSinTpl t of
     (PrfHasSin , PrfHasSin)  -> Tpl  <$@> FGV.Exp (fst v)
                                      <*@> FGV.Exp (snd v)
    TFG.Ary ta                -> case TFG.getPrfHasSinAry t of
      PrfHasSin
        | fst (bounds v) == 0 -> Ary  <$@> (FGV.Exp . (+ 1) . snd . bounds) v
                                      <*@> (samTyp (TFG.Arr TFG.Wrd ta)
                                            (FGV.Exp (fromJust
                                                    . flip lookup (assocs v))))
        | otherwise           -> fail "Bad Array!"
    TFG.Cmx                   -> Cmx <$@> FGV.Exp (realPart v)
                                     <*@> FGV.Exp (imagPart v)
    TFG.Vct _                 -> impossibleM
    TFG.May _                 -> impossibleM


instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb , r ~ r' , ta ~ ta' , tb ~ tb')=>
         Cnv (FGV.Exp (ta' -> tb') , Env FGV.Exp r') (Exp r ta -> Exp r tb)
         where
  cnv (FGV.Exp f , r) = let ?r = r in
    pure (frmRgtZro . cnvImp . (fmap f :: FGV.Exp ta -> FGV.Exp tb) . frmRgtZro . cnvImp)
