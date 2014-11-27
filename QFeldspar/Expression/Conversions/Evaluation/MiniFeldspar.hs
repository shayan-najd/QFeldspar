{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.Evaluation.MiniFeldspar () where

import QFeldspar.MyPrelude
import QFeldspar.Expression.MiniFeldspar
import qualified QFeldspar.Expression.GADTValue as FGV
import qualified QFeldspar.Type.GADT            as TFG
import QFeldspar.Variable.Typed
import QFeldspar.Environment.Typed hiding (fmap)
import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()
import QFeldspar.Singleton
import QFeldspar.Expression.Utils.Common

instance (HasSin TFG.Typ t, r' ~ r , t' ~ t) =>
         Cnv (Exp r t , Env FGV.Exp r) (FGV.Exp t')
         where
  cnv (ee , r) = let ?r = r in let t = sin :: TFG.Typ t in case ee of
    Tmp _                    -> fail "Not Supported!"
    Mul er ei                -> case t of
                     TFG.Int -> FGV.mul  <$@> er <*@> ei
                     TFG.Flt -> FGV.mul  <$@> er <*@> ei
                     _       -> fail "Type Error in Mul"
    AppV (v :: Var rv tv) es -> appV (T :: T tv) <$@> v <*@>
                                (T :: T tv , es)
    Tag _  e                 -> cnvImp e
    Let el eb                -> FGV.leT  <$@> el <*@> eb
    _  -> $(biRecAppMQS 'ee ''Exp "FGV" ['Tmp,'Mul,'AppV,'Tag,'Let]
            (trvWrp 't))

instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb , ta' ~ ta , tb' ~ tb) =>
         Cnv (Exp r ta -> Exp r tb , Env FGV.Exp r) (FGV.Exp (Arr ta' tb'))
         where
  cnv (f , r)  =  let ?r = r in pure (FGV.Exp (FGV.getTrm
                                             . frmRgtZro . cnvImp
                                             . f
                                             . frmRgtZro . cnvImp
                                             . FGV.Exp ))

instance (HasSin TFG.Typ t , t' ~ TFG.Arg t) =>
         Cnv ((T t , Env (Exp r) t') , Env FGV.Exp r) (Env FGV.Exp   t')
         where
  cnv ((T , vss) , r) = let ?r = r in let t = sin :: TFG.Typ t in
                                      case (t , vss) of
    (TFG.Arr ta tb , Ext v vs) -> case TFG.getPrfHasSinArr t of
      (PrfHasSin , PrfHasSin)  -> Ext <$@> samTyp ta v <*@> (samTyp tb T , vs)
    (TFG.Arr _ _   , _)        -> impossibleM
    (TFG.Int       , Emp)      -> pure Emp
    (TFG.Int       , _)        -> impossibleM
    (TFG.Bol       , Emp)      -> pure Emp
    (TFG.Bol       , _)        -> impossibleM
    (TFG.Flt       , Emp)      -> pure Emp
    (TFG.Flt       , _)        -> impossibleM
    (TFG.Tpl _  _  , Emp)      -> pure Emp
    (TFG.Tpl _  _  , _)        -> impossibleM
    (TFG.Ary _     , Emp)      -> pure Emp
    (TFG.Ary _     , _)        -> impossibleM
    (TFG.Vct _     , Emp)      -> pure Emp
    (TFG.Vct _     , _)        -> impossibleM
    (TFG.Cmx       , Emp)      -> pure Emp
    (TFG.Cmx       , _)        -> impossibleM
    (TFG.May _     , Emp)      -> pure Emp
    (TFG.May _     , _)        -> impossibleM

appV :: forall t. HasSin TFG.Typ t => T t ->
          FGV.Exp t -> Env FGV.Exp (TFG.Arg t) -> FGV.Exp (TFG.Out t)
appV T vv vss  = let t = sin :: TFG.Typ t in case (t , vss) of
  (TFG.Arr _ tb , Ext v vs) -> case TFG.getPrfHasSinArr t of
    (PrfHasSin , PrfHasSin) -> appV T (samTyp tb (FGV.app vv v)) vs
  (TFG.Int       , Emp)     -> vv
  (TFG.Bol       , Emp)     -> vv
  (TFG.Flt       , Emp)     -> vv
  (TFG.Tpl _  _  , Emp)     -> vv
  (TFG.Ary _     , Emp)     -> vv
  (TFG.Cmx       , Emp)     -> vv
  (TFG.May _     , Emp)     -> vv
  (TFG.Vct _     , Emp)     -> vv

instance (HasSin TFG.Typ t , r ~ r' , t ~ t') =>
         Cnv (FGV.Exp t' , Env FGV.Exp r') (Exp r t)
         where
  cnv (FGV.Exp v , r) = let ?r = r in let t = sin :: TFG.Typ t in case t of
    TFG.Int                   -> pure (ConI v)
    TFG.Bol                   -> pure (ConB v)
    TFG.Flt                   -> pure (ConF v)
    TFG.Tpl _ _               -> case TFG.getPrfHasSinTpl (T :: T t) of
     (PrfHasSin , PrfHasSin)  -> Tpl  <$@> FGV.Exp (fst v) <*@> FGV.Exp (snd v)
    TFG.Ary ta                -> case TFG.getPrfHasSinAry (T :: T t) of
      PrfHasSin
        | fst (bounds v) == 0 -> Ary  <$@> (FGV.Exp . (+ 1) . snd . bounds) v
                                      <*@> samTyp (TFG.Arr TFG.Int ta)
                                            (FGV.Exp (fromJust
                                                    . flip lookup (assocs v)))
        | otherwise           -> fail "Bad Array!"
    TFG.Cmx                   -> Cmx <$@> FGV.Exp (realPart v)
                                     <*@> FGV.Exp (imagPart v)
    TFG.Arr _ _               -> fail "Type Error!"
    TFG.May _                 -> fail "Type Error!"
    TFG.Vct _                 -> fail "Type Error!"


instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb , r ~ r' , ta ~ ta' , tb ~ tb')=>
         Cnv (FGV.Exp (Arr ta' tb') , Env FGV.Exp r') (Exp r ta -> Exp r tb)
         where
  cnv (FGV.Exp f , r) = let ?r = r in
    pure ( frmRgtZro  . cnvImp
         . (fmap :: (a -> b) -> FGV.Exp a -> FGV.Exp b)  f
         . frmRgtZro  . cnvImp)
