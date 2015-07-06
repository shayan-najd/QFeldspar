module QFeldspar.Expression.Conversions.Evaluation.MiniFeldspar () where

import QFeldspar.MyPrelude
import QFeldspar.Expression.MiniFeldspar
import qualified QFeldspar.Expression.GADTValue as FGV
import qualified QFeldspar.Type.GADT            as TG
import QFeldspar.Environment.Typed hiding (fmap)
import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()
import QFeldspar.Singleton
import QFeldspar.Expression.Utils.Common
import qualified QFeldspar.Prelude.Haskell as PH

instance (TG.Type a, a ~ a') =>
         Cnv (Exp s a , Env FGV.Exp s) (FGV.Exp a')
         where
  cnv (ee , s) = let t = sin :: TG.Typ a in case ee of
    Tmp _    -> impossibleM
    Prm x es -> FGV.prm (get x s) <$> TG.mapMC (cnvWth s) es
    _        -> $(biGenOverloadedMWL 'ee ''Exp "FGV" ['Tmp,'Prm]
                  (trvWrp 't)
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
                           [|cnvWth s |]
                       | otherwise                -> [| pure |]))

instance (TG.Type t , r ~ r' , t ~ t') =>
         Cnv (FGV.Exp t' , Env FGV.Exp r') (Exp r t)
         where
  cnv (FGV.Exp v , r) = let t = sin :: TG.Typ t in case t of
    TG.Wrd                   -> pure (ConI v)
    TG.Bol                   -> pure (ConB v)
    TG.Flt                   -> pure (ConF v)
    TG.Tpl _ _               -> case TG.getPrfHasSinTpl t of
     (PrfHasSin , PrfHasSin) -> Tpl  <$> cnv (FGV.Exp (fst v) , r)
                                     <*> cnv (FGV.Exp (snd v) , r)
    TG.Ary _                 -> case TG.getPrfHasSinAry t of
      PrfHasSin              -> Ary  <$> cnv (FGV.Exp (PH.lnArr v) , r)
                                     <*> cnv (FGV.Exp (PH.ixArr v) , r)
    TG.Cmx                   -> Cmx  <$> cnv (FGV.Exp (realPart v) , r)
                                     <*> cnv (FGV.Exp (imagPart v) , r)
    TG.Arr _ _               -> fail "Type Error!"
    TG.May _                 -> fail "Type Error!"
    TG.Vct _                 -> fail "Type Error!"
    TG.Int                   -> fail "Type Error!"
    TG.Rat                   -> fail "Type Error!"
    TG.Chr                   -> fail "Type Error!"
    TG.Str                   -> fail "Type Error!"
    TG.TVr _                 -> fail "Type Error!"

instance (TG.Type ta , TG.Type tb , r ~ r' , ta ~ ta' , tb ~ tb')=>
         Cnv (FGV.Exp (ta' -> tb') , Env FGV.Exp r') (Exp r ta -> Exp r tb)
         where
  cnv (FGV.Exp f , r) =
    pure ( frmRgtZro
           . cnvWth r
           . (fmap :: (a -> b) -> FGV.Exp a -> FGV.Exp b)  f
           . frmRgtZro
           . cnvWth r)
