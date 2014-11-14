{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Feldspar.Conversions.Normalisation () where

import QFeldspar.MyPrelude
import QFeldspar.Expression.Feldspar.Utils.Common
import qualified QFeldspar.Expression.Feldspar.GADTHigherOrder as FGHO
import qualified QFeldspar.Expression.Feldspar.MiniFeldspar  as FMWS

import qualified QFeldspar.Type.Feldspar.GADT                  as TFG

import QFeldspar.Variable.Typed
import QFeldspar.Environment.Typed hiding (fmap)

import QFeldspar.Conversion
import QFeldspar.Singleton

instance (HasSin TFG.Typ t , t ~ t' , r ~ r') =>
         Cnv (FGHO.Exp r t , rr) (FMWS.Exp r' t') where
  cnv (ee , r) = let ?r = r in let t = (sin :: TFG.Typ t) in case ee of
    FGHO.Var v                -> case sin :: TFG.Typ t of
      TFG.Int                 -> pure (FMWS.AppV v Emp)
      TFG.Bol                 -> pure (FMWS.AppV v Emp)
      TFG.Flt                 -> pure (FMWS.AppV v Emp)
      TFG.Tpl _ _             -> pure (FMWS.AppV v Emp)
      TFG.Ary _               -> pure (FMWS.AppV v Emp)
      TFG.Cmx                 -> pure (FMWS.AppV v Emp)
      TFG.Vct _               -> fail "Normalisation Error!"
      TFG.Arr _ _             -> fail "Normalisation Error!"
      TFG.May _               -> fail "Normalisation Error!"
    FGHO.Abs _                -> fail "Normalisation Error!"
    FGHO.Non                  -> fail "Normalisation Error!"
    FGHO.Som _                -> fail "Normalisation Error!"
    FGHO.May _ _ _            -> fail "Normalisation Error!"
    FGHO.AryV _ _             -> fail "Normalisation Error!"
    FGHO.LenV _               -> fail "Normalisation Error!"
    FGHO.IndV _ _             -> fail "Normalisation Error!"
    FGHO.App _ _              -> do Exs1 v tv <- getVar ee
                                    PrfHasSin <- getPrfHasSinM tv
                                    DblExsSin es tys <- getArg ee
                                      (DblExsSin Emp Emp)
                                    TFG.EqlOut <- TFG.eqlOut t tv
                                    TFG.EqlArg <- TFG.eqlArg tys tv
                                    pure (FMWS.AppV v es)
    _                         -> $(biRecAppMQW 'ee ''FGHO.Exp "FMWS"
     ['FGHO.Var,'FGHO.Abs,'FGHO.App,'FGHO.Non,'FGHO.Som,'FGHO.May
     ,'FGHO.AryV,'FGHO.LenV,'FGHO.IndV] (trvWrp 't))

instance (HasSin TFG.Typ a , HasSin TFG.Typ b, a ~ a' , b ~ b' , r ~ r') =>
    Cnv (FGHO.Exp r' (Arr a' b') , rr) (FMWS.Exp r a -> FMWS.Exp r b)  where
    cnv (ee , r) = case ee of
      FGHO.Abs e -> cnv (e , r)
      _          -> fail "Normalisation Error!"

instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb , r ~ r' , ta ~ ta' ,tb ~ tb') =>
         Cnv (FGHO.Exp r  ta  -> FGHO.Exp r  tb , rr)
             (FMWS.Exp r' ta' -> FMWS.Exp r' tb')
         where
  cnv (ee , r) = let ?r = r in
    pure (frmRgt . cnvImp . ee . frmRgt . cnvImp)

instance (HasSin TFG.Typ t , t' ~ t , r' ~ r) =>
         Cnv (FMWS.Exp r' t' , rr) (FGHO.Exp r t)  where
  cnv (ee , r) = let ?r = r in let t = (sin :: TFG.Typ t) in case ee of
    FMWS.AppV v es            -> case (sinTyp v , es) of
      (TFG.Arr _ _ , Ext _ _) -> do Exs1 e te <- fldApp (FGHO.Var v) es
                                    Rfl <- eqlSin te t
                                    pure e
      (TFG.Int     , Emp)     -> pure (FGHO.Var v)
      (TFG.Bol     , Emp)     -> pure (FGHO.Var v)
      (TFG.Flt     , Emp)     -> pure (FGHO.Var v)
      (TFG.Tpl _ _ , Emp)     -> pure (FGHO.Var v)
      (TFG.Ary _   , Emp)     -> pure (FGHO.Var v)
      (TFG.Vct _   , Emp)     -> pure (FGHO.Var v)
      (TFG.Cmx     , Emp)     -> pure (FGHO.Var v)
      (TFG.May _   , Emp)     -> pure (FGHO.Var v)
    FMWS.Tag _  e             -> cnvImp e
    _                         ->
      $(biRecAppMQW 'ee ''FMWS.Exp "FGHO" ['FMWS.AppV,'FMWS.Tag] (trvWrp 't))

instance (HasSin TFG.Typ a , HasSin TFG.Typ b, a ~ a' , b ~ b' , r ~ r') =>
    Cnv (FMWS.Exp r a -> FMWS.Exp r b , rr) (FGHO.Exp r' (Arr a' b')) where
    cnv (ee , r) = fmap FGHO.Abs (cnv (ee , r))

instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb, ta ~ ta' , tb ~ tb' , r ~ r') =>
         Cnv (FMWS.Exp r  ta  -> FMWS.Exp r  tb , rr)
             (FGHO.Exp r' ta' -> FGHO.Exp r' tb')
         where
  cnv (ee , r) = let ?r = r in
    pure (frmRgt . cnvImp . ee . frmRgt . cnvImp)

fldApp :: forall r t ta tb . (t ~ (Arr ta tb) , HasSin TFG.Typ t) =>
          FGHO.Exp r t ->
          Env (FMWS.Exp r) (ta ': TFG.Arg tb) ->
          ErrM (Exs1 (FGHO.Exp r) TFG.Typ)
fldApp e ess = let ?r = () in case TFG.getPrfHasSinArr (T :: T t) of
  (PrfHasSin , PrfHasSin) -> case (sin :: TFG.Typ t , ess) of
    (TFG.Arr _ (TFG.Arr _ _) , Ext ea es@(Ext _ _)) -> do
      ea' <- cnvImp ea
      fldApp (FGHO.App e ea') es
    (TFG.Arr _ tb            , Ext ea Emp)          -> do
      ea' <- cnvImp ea
      pure (Exs1 (FGHO.App e ea') tb)
    _                                               ->
      impossibleM

getVar :: forall r t. HasSin TFG.Typ t =>
          FGHO.Exp r t -> ErrM (Exs1 (Var r) TFG.Typ)
getVar e = case e of
  FGHO.App (FGHO.Var v)       _ -> pure (Exs1 v (sinTyp v))
  FGHO.App ef@(FGHO.App _  _) _ -> getVar ef
  _                             -> fail "Normalisation Error!"

data DblExsSin :: (ka -> kb -> *) -> ka -> ka -> * where
  DblExsSin :: c2 tf1 t -> c2 tf2 t -> DblExsSin c2 tf1 tf2

getArg :: forall r t. FGHO.Exp r t -> DblExsSin Env (FMWS.Exp r) TFG.Typ ->
          ErrM (DblExsSin Env (FMWS.Exp r) TFG.Typ)
getArg e (DblExsSin args tys) = let ?r = () in case e of
  FGHO.App (FGHO.Var _)       ea -> do
    ea' <- cnvImp ea
    pure (DblExsSin (Ext ea' args) (Ext (sinTyp ea) tys))
  FGHO.App ef@(FGHO.App _ _) ea -> do
    ea' <- cnvImp ea
    getArg ef (DblExsSin (Ext ea' args) (Ext (sinTyp ea) tys))
  _                              ->
    fail "Normalisation Error!"
