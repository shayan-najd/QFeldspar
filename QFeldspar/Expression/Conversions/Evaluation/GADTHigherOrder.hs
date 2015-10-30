module QFeldspar.Expression.Conversions.Evaluation.GADTHigherOrder () where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTHigherOrder
import qualified QFeldspar.Expression.GADTValue as FGV
import qualified QFeldspar.Type.GADT as TG
import QFeldspar.Environment.Typed hiding (fmap)
import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()
import QFeldspar.Singleton
import QFeldspar.Literal.GADT
import qualified QFeldspar.Prelude.Haskell as PH

instance (TG.Type a , a ~ a') =>
         Cnv (Exp s a , Env FGV.Exp s) (FGV.Exp a') where
  cnv (ee , s) = case ee of
    Tmp _    -> impossibleM
    Prm x es -> FGV.prm  (get x s) <$>
                TG.mapMC (cnvWth s)  es
    _  -> $(biGenOverloadedML 'ee ''Exp "FGV"
            ['Tmp,'Prm]
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

instance (TG.Type t , r ~ r' , t ~ t') =>
         Cnv (FGV.Exp t' , Env FGV.Exp r') (Exp r t)
         where
  cnv (FGV.Exp v , r) = let t = sin :: TG.Typ t in case t of
    TG.Wrd                   -> pure (Int (toInteger v))
    TG.Bol                   -> pure (ConB v)
    TG.Flt                   -> pure (Rat (toRational v))
    TG.Arr (_ :: TG.Typ b) (_ :: TG.Typ c) -> case TG.getPrfHasSinArr t of
     (PrfHasSin , PrfHasSin)  -> Abs  <$> pure (frmRgtZro
                                                . cnvWth r
                                                . (fmap v :: FGV.Exp b
                                                          -> FGV.Exp c)
                                                . frmRgtZro
                                                . cnvWth r)
    TG.Tpl _ _               -> case TG.getPrfHasSinTpl t of
     (PrfHasSin , PrfHasSin) -> Tpl  <$> cnv (FGV.Exp (fst v) , r)
                                     <*> cnv (FGV.Exp (snd v) , r)
    TG.Ary _                 -> case TG.getPrfHasSinAry t of
      PrfHasSin              -> Ary  <$> cnv (FGV.Exp (PH.lnArr v) , r)
                                     <*> cnv (FGV.Exp (PH.ixArr v) , r)
    TG.Cmx                   -> Cmx  <$> cnv (FGV.Exp (realPart v) , r)
                                     <*> cnv (FGV.Exp (imagPart v) , r)
    TG.Vct _                 -> case TG.getPrfHasSinVec t of
      PrfHasSin              -> AryV <$> cnv (FGV.Exp (PH.lnVec v) , r)
                                     <*> cnv (FGV.Exp (PH.ixVec v) , r)
    TG.May _                 -> case TG.getPrfHasSinMay t of
      PrfHasSin              -> case v of
        Just x               -> Som <$> cnv (FGV.Exp x , r)
        Nothing              -> pure Non
    TG.Int                   -> pure (Lit (IntegerL v))
    TG.Rat                   -> pure (Lit (RationalL v))
    TG.Chr                   -> pure (Lit (CharL v))
    TG.Str                   -> pure (Lit (StringL v))
    TG.TVr _                 -> impossibleM
