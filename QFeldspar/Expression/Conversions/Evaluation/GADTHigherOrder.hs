{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.Evaluation.GADTHigherOrder () where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTHigherOrder
import qualified QFeldspar.Expression.GADTValue as FGV
import qualified QFeldspar.Type.GADT as TG
import QFeldspar.Environment.Typed hiding (fmap)
import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()
import QFeldspar.Singleton
import QFeldspar.Expression.Utils.Common

instance (HasSin TG.Typ a , a ~ a') =>
         Cnv (Exp s a , Env FGV.Exp s) (FGV.Exp a') where
  cnv (ee , s) = let t = sin :: TG.Typ a in case ee of
    Tmp _    -> impossibleM
    Prm x es -> FGV.prm  (get x s) <$>
                TG.mapMC (cnvWth s)  es
    _  -> $(biGenOverloadedMWL 'ee ''Exp "FGV"
            ['Tmp,'Prm] (trvWrp 't)
            (\ tt -> if
                 | matchQ tt [t| Exp a a -> Exp a a |] ->
                     [| \ f -> pure
                               (FGV.Exp
                                (FGV.getTrm
                                 . frmRgtZro
                                 . cnvWth s
                                 . f
                                 . frmRgtZro
                                 . cnvWth s
                                 . FGV.Exp )) |]
                 | matchQ tt [t| Exp a a |] ->
                     [| \ e -> cnv (e , s) |]
                 | otherwise                -> [| pure |]))

instance (HasSin TG.Typ t , r ~ r' , t ~ t') =>
         Cnv (FGV.Exp t' , Env FGV.Exp r') (Exp r t)
         where
  cnv (FGV.Exp v , r) = let t = sin :: TG.Typ t in case t of
    TG.Wrd                   -> pure (ConI v)
    TG.Bol                   -> pure (ConB v)
    TG.Flt                   -> pure (ConF v)
    TG.Arr (_ :: TG.Typ b) (_ :: TG.Typ c) -> case TG.getPrfHasSinArr t of
     (PrfHasSin , PrfHasSin)  -> Abs  <$> pure (frmRgtZro
                                                . cnvWth r
                                                . (fmap v :: FGV.Exp b -> FGV.Exp c)
                                                . frmRgtZro
                                                . cnvWth r)
    TG.Tpl _ _               -> case TG.getPrfHasSinTpl t of
     (PrfHasSin , PrfHasSin)  -> Tpl  <$> cnv (FGV.Exp (fst v) , r)
                                      <*> cnv (FGV.Exp (snd v) , r)
    TG.Ary ta                -> case TG.getPrfHasSinAry t of
      PrfHasSin
        | fst (bounds v) == 0 -> Ary  <$> cnv ((FGV.Exp . (+ 1) . snd . bounds) v , r)
                                      <*> cnv ((samTyp (TG.Arr TG.Wrd ta)
                                                (FGV.Exp (fromJust
                                                          . flip lookup (assocs v)))) , r)
        | otherwise           -> fail "Bad Array!"
    TG.Cmx                   -> Cmx <$> cnv (FGV.Exp (realPart v) , r)
                                     <*> cnv (FGV.Exp (imagPart v) , r)
    TG.Vct _                 -> impossibleM
    TG.May _                 -> impossibleM
