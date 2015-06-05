{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.Normalisation () where

import QFeldspar.MyPrelude
import QFeldspar.Expression.Utils.Common
import qualified QFeldspar.Expression.GADTHigherOrder as FGHO
import qualified QFeldspar.Expression.MiniFeldspar  as FMWS

import qualified QFeldspar.Type.GADT                  as TFG

-- import QFeldspar.Variable.Typed
-- import QFeldspar.Environment.Typed hiding (fmap)

import QFeldspar.Conversion
import QFeldspar.Singleton

instance (HasSin TFG.Typ t , t ~ t' , r ~ r') =>
         Cnv (FGHO.Exp r t , rr) (FMWS.Exp r' t') where
  cnv (ee , r) = let t = (sin :: TFG.Typ t) in case ee of
    FGHO.Abs _                -> fail "Normalisation Error!"
    FGHO.App _ _              -> fail "Normalisation Error!"
    FGHO.Non                  -> fail "Normalisation Error!"
    FGHO.Som _                -> fail "Normalisation Error!"
    FGHO.May _ _ _            -> fail "Normalisation Error!"
    FGHO.AryV _ _             -> fail "Normalisation Error!"
    FGHO.LenV _               -> fail "Normalisation Error!"
    FGHO.IndV _ _             -> fail "Normalisation Error!"
    FGHO.Int  _               -> fail "Normalisation Error!"
    FGHO.Fix  _               -> fail "Normalisation Error!"
    FGHO.Prm x ns             -> FMWS.Prm x <$> TFG.mapMC (cnvWth r) ns
    _                         -> $(biGenOverloadedMW 'ee ''FGHO.Exp "FMWS"
     ['FGHO.Prm,'FGHO.Abs,'FGHO.App,'FGHO.Non,'FGHO.Som,'FGHO.May
     ,'FGHO.AryV,'FGHO.LenV,'FGHO.IndV,'FGHO.Int,'FGHO.Fix] (trvWrp 't) (const [| cnvWth r |]))

instance (HasSin TFG.Typ a , HasSin TFG.Typ b, a ~ a' , b ~ b' , r ~ r') =>
    Cnv (FGHO.Exp r' (a' -> b') , rr) (FMWS.Exp r a -> FMWS.Exp r b)  where
    cnv (ee , r) = case ee of
      FGHO.Abs e -> cnv (e , r)
      _          -> fail "Normalisation Error!"

instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb , r ~ r' , ta ~ ta' ,tb ~ tb') =>
         Cnv (FGHO.Exp r  ta  -> FGHO.Exp r  tb , rr)
             (FMWS.Exp r' ta' -> FMWS.Exp r' tb')
         where
  cnv (ee , r) =
    pure (frmRgtZro . cnvWth r . ee . frmRgtZro . cnvWth r)

instance (HasSin TFG.Typ t , t' ~ t , r' ~ r) =>
         Cnv (FMWS.Exp r' t' , rr) (FGHO.Exp r t)  where
  cnv (ee , r) = let t = sin :: TFG.Typ t in
    case ee of
      FMWS.Prm x ns -> FGHO.Prm x <$> TFG.mapMC (cnvWth r) ns
      _             -> $(biGenOverloadedMW 'ee ''FMWS.Exp "FGHO" ['FMWS.Prm]
                            (trvWrp 't) (const [| cnvWth r |]))

instance (HasSin TFG.Typ a , HasSin TFG.Typ b, a ~ a' , b ~ b' , r ~ r') =>
    Cnv (FMWS.Exp r a -> FMWS.Exp r b , rr) (FGHO.Exp r' (a' -> b')) where
    cnv (ee , r) = fmap FGHO.Abs (cnv (ee , r))

instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb, ta ~ ta' , tb ~ tb' , r ~ r') =>
         Cnv (FMWS.Exp r  ta  -> FMWS.Exp r  tb , rr)
             (FGHO.Exp r' ta' -> FGHO.Exp r' tb')
         where
  cnv (ee , r) = pure (frmRgtZro . cnvWth r . ee . frmRgtZro . cnvWth r)
{-

fldApp :: forall r t ta tb . (t ~ (ta -> tb) , HasSin TFG.Typ t) =>
          FGHO.Exp r t ->
          Env (FMWS.Exp r) (ta ': TFG.Arg tb) ->
          NamM ErrM (Exs1 (FGHO.Exp r) TFG.Typ)
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
          FGHO.Exp r t -> NamM ErrM (Exs1 (Var r) TFG.Typ)
getVar e = case e of
  FGHO.App (FGHO.Var v)       _ -> pure (Exs1 v (sinTyp v))
  FGHO.App ef@(FGHO.App _  _) _ -> getVar ef
  _                             -> fail "Normalisation Error!"

data DblExsSin :: (ka -> kb -> *) -> ka -> ka -> * where
  DblExsSin :: c2 tf1 t -> c2 tf2 t -> DblExsSin c2 tf1 tf2

getArg :: forall r t. FGHO.Exp r t -> DblExsSin Env (FMWS.Exp r) TFG.Typ ->
          NamM ErrM (DblExsSin Env (FMWS.Exp r) TFG.Typ)
getArg e (DblExsSin args tys) = let ?r = () in case e of
  FGHO.App (FGHO.Var _)       ea -> do
    ea' <- cnvImp ea
    pure (DblExsSin (Ext ea' args) (Ext (sinTyp ea) tys))
  FGHO.App ef@(FGHO.App _ _) ea -> do
    ea' <- cnvImp ea
    getArg ef (DblExsSin (Ext ea' args) (Ext (sinTyp ea) tys))
  _                              ->
    fail "Normalisation Error!"
-}
