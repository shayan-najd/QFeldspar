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

instance (HasSin TFG.Typ a , a ~ a') =>
         Cnv (Exp s a , Env FGV.Exp s) (FGV.Exp a') where
  cnv (ee , s) = let t = sin :: TFG.Typ a in case ee of
    Tmp _    -> impossibleM
    Prm x es -> FGV.prm  (get x s) <$>
                TFG.mapMC (sinTyp x) (\ e -> cnv (e , s))  es
    _  -> $(biGenOverloadedMWL 'ee ''Exp "FGV"
            ['Tmp,'Prm] (trvWrp 't)
            (\ tt -> if
                 | matchQ tt [t| Exp a a -> Exp a a |] ->
                     [| \ f -> pure
                               (FGV.Exp
                                (FGV.getTrm
                                 . frmRgtZro
                                 . (\ e -> cnv (e , s))
                                 . f
                                 . frmRgtZro
                                 . (\ e -> cnv (e , s))
                                 . FGV.Exp )) |]
                 | matchQ tt [t| Exp a a |] ->
                     [| \ e -> cnv (e , s) |]
                 | otherwise                -> [| pure |]))

instance (HasSin TFG.Typ t , r ~ r' , t ~ t') =>
         Cnv (FGV.Exp t' , Env FGV.Exp r') (Exp r t)
         where
  cnv (FGV.Exp v , r) = let t = sin :: TFG.Typ t in case t of
    TFG.Wrd                   -> pure (ConI v)
    TFG.Bol                   -> pure (ConB v)
    TFG.Flt                   -> pure (ConF v)
    TFG.Arr (_ :: TFG.Typ b) (_ :: TFG.Typ c) -> case TFG.getPrfHasSinArr t of
     (PrfHasSin , PrfHasSin)  -> Abs  <$> pure (frmRgtZro
                                                . (\ e -> cnv (e , r))
                                                . (fmap v :: FGV.Exp b -> FGV.Exp c)
                                                . frmRgtZro
                                                . (\ e -> cnv (e , r)))
    TFG.Tpl _ _               -> case TFG.getPrfHasSinTpl t of
     (PrfHasSin , PrfHasSin)  -> Tpl  <$> cnv (FGV.Exp (fst v) , r)
                                      <*> cnv (FGV.Exp (snd v) , r)
    TFG.Ary ta                -> case TFG.getPrfHasSinAry t of
      PrfHasSin
        | fst (bounds v) == 0 -> Ary  <$> cnv ((FGV.Exp . (+ 1) . snd . bounds) v , r)
                                      <*> cnv ((samTyp (TFG.Arr TFG.Wrd ta)
                                                (FGV.Exp (fromJust
                                                          . flip lookup (assocs v)))) , r)
        | otherwise           -> fail "Bad Array!"
    TFG.Cmx                   -> Cmx <$> cnv (FGV.Exp (realPart v) , r)
                                     <*> cnv (FGV.Exp (imagPart v) , r)
    TFG.Vct _                 -> impossibleM
    TFG.May _                 -> impossibleM
