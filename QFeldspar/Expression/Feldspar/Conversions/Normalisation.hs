module QFeldspar.Expression.Feldspar.Conversions.Normalisation () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Expression.Feldspar.GADTHigherOrder as FGHO
import qualified QFeldspar.Expression.Feldspar.MiniFeldspar  as FMWS

import qualified QFeldspar.Type.Feldspar.GADT                  as TFG

import QFeldspar.Variable.Typed
import QFeldspar.Environment.Typed

import QFeldspar.Conversion
import QFeldspar.Singleton

instance (HasSin TFG.Typ t , t ~ t' , r ~ r') =>
         Cnv (FGHO.Exp r t , rr) (FMWS.Exp r' t') where
  cnv (ee , r) = let ?r = r in let t = (sin :: TFG.Typ t) in case ee of
    FGHO.ConI i               -> pure (FMWS.ConI i)
    FGHO.ConB b               -> pure (FMWS.ConB b)
    FGHO.ConF f               -> pure (FMWS.ConF f)
    FGHO.Var v                -> case sin :: TFG.Typ t of
      TFG.Int                 -> pure (FMWS.AppV v Emp)
      TFG.Bol                 -> pure (FMWS.AppV v Emp)
      TFG.Flt                 -> pure (FMWS.AppV v Emp)
      TFG.Arr _ _             -> fail "Normalisation Error!"
      TFG.Tpl _ _             -> pure (FMWS.AppV v Emp)
      TFG.Ary _               -> pure (FMWS.AppV v Emp)
      TFG.Vct _               -> pure (FMWS.AppV v Emp)
      TFG.Cmx                 -> pure (FMWS.AppV v Emp)
      TFG.May _               -> pure (FMWS.AppV v Emp)
    FGHO.Abs _                -> fail "Normalisation Error!"
    FGHO.App _ _              -> do Exs1 v tv <- getVar ee
                                    PrfHasSin <- getPrfHasSinM tv
                                    DblExsSin es tys <- getArg ee
                                      (DblExsSin Emp Emp)
                                    TFG.EqlOut <- TFG.eqlOut t tv
                                    TFG.EqlArg <- TFG.eqlArg tys tv
                                    pure (FMWS.AppV v es)
    FGHO.Cnd ec et ef         -> FMWS.Cnd <$@> ec <*@> et <*@> ef
    FGHO.Whl ec eb ei         -> FMWS.Whl <$@> ec <*@> eb <*@> ei
    FGHO.Tpl ef es            -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin) -> FMWS.Tpl <$@> ef <*@> es
    FGHO.Fst e                -> FMWS.Fst <$@> e
    FGHO.Snd e                -> FMWS.Snd <$@> e
    FGHO.Ary el ef            -> case TFG.getPrfHasSinAry t of
      PrfHasSin               -> FMWS.Ary <$@> el <*@> ef
    FGHO.Len ea               -> FMWS.Len <$@> ea
    FGHO.Ind ea ei            -> FMWS.Ind <$@> ea <*@> ei
    FGHO.AryV _ _             -> fail "Normalisation Error!"
    FGHO.LenV _               -> fail "Normalisation Error!"
    FGHO.IndV _ _             -> fail "Normalisation Error!"
    FGHO.Let el eb            -> FMWS.Let <$@> el <*@> eb
    FGHO.Cmx er ei            -> FMWS.Cmx <$@> er <*@> ei
    FGHO.Tmp x                -> pure (FMWS.Tmp x)
    FGHO.Non                  -> fail "Normalisation Error!"
    FGHO.Som _                -> fail "Normalisation Error!"
    FGHO.May _ _ _            -> fail "Normalisation Error!"
    FGHO.Mul er ei            -> FMWS.Mul <$@> er <*@> ei

instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb , r ~ r' , ta ~ ta' ,tb ~ tb') =>
         Cnv (FGHO.Exp r  ta  -> FGHO.Exp r  tb , rr)
             (FMWS.Exp r' ta' -> FMWS.Exp r' tb')
         where
  cnv (ee , r) = let ?r = r in
    pure (frmRgt . cnvImp . ee . frmRgt . cnvImp)

instance (HasSin TFG.Typ t , t' ~ t , r' ~ r) =>
         Cnv (FMWS.Exp r' t' , rr) (FGHO.Exp r t)  where
  cnv (ee , r) = let ?r = r in let t = (sin :: TFG.Typ t) in case ee of
    FMWS.ConI i               -> pure (FGHO.ConI i)
    FMWS.ConB b               -> pure (FGHO.ConB b)
    FMWS.ConF f               -> pure (FGHO.ConF f)
    FMWS.AppV v es            -> case (sinTyp v , es) of
      (TFG.Int     , Emp)     -> FGHO.Var <$> pure v
      (TFG.Bol     , Emp)     -> FGHO.Var <$> pure v
      (TFG.Flt     , Emp)     -> FGHO.Var <$> pure v
      (TFG.Arr _ _ , Ext _ _) -> do Exs1 e te <- fldApp (FGHO.Var v) es
                                    Rfl <- eqlSin te t
                                    pure e
      (TFG.Tpl _ _ , Emp)     -> FGHO.Var <$> pure v
      (TFG.Ary _   , Emp)     -> FGHO.Var <$> pure v
      (TFG.Vct _   , Emp)     -> FGHO.Var <$> pure v
      (TFG.Cmx     , Emp)     -> FGHO.Var <$> pure v
      (TFG.May _   , Emp)     -> FGHO.Var <$> pure v
    FMWS.Cnd ec et ef         -> FGHO.Cnd <$@> ec <*@> et <*@> ef
    FMWS.Whl ec eb ei         -> FGHO.Whl <$@> ec <*@> eb <*@> ei
    FMWS.Tpl ef es            -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin) -> FGHO.Tpl <$@> ef <*@> es
    FMWS.Fst e                -> FGHO.Fst <$@> e
    FMWS.Snd e                -> FGHO.Snd <$@> e
    FMWS.Ary el ef            -> case TFG.getPrfHasSinAry t of
      PrfHasSin               -> FGHO.Ary <$@> el <*@> ef
    FMWS.Len ea               -> FGHO.Len <$@> ea
    FMWS.Ind ea ei            -> FGHO.Ind <$@> ea <*@> ei
    FMWS.Let el eb            -> FGHO.Let <$@> el <*@> eb
    FMWS.Cmx er ei            -> FGHO.Cmx <$@> er <*@> ei
    FMWS.Tag _  e             -> cnvImp e
    FMWS.Tmp x                -> pure (FGHO.Tmp x)
    FMWS.Mul er ei            -> FGHO.Mul <$@> er <*@> ei

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
