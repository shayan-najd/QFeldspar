{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.Evaluation.GADTTyped () where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTTyped
import qualified QFeldspar.Expression.ADTValue as FAV
import QFeldspar.Environment.Scoped
import QFeldspar.Nat.ADT
import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()

instance Cnv (Exp m n t , (Env m FAV.Exp , Env n FAV.Exp)) FAV.Exp where
  cnv (ee , r@(s , g)) = join (case ee of
    Prm _ x es   -> FAV.prm (get x s) <$> mapM (\ e -> cnv (e , r))  es
    Var x        -> pure (pure (get x g))
    App _  ef ea -> FAV.app  <$> cnv (ef , r) <*> cnv (ea , r)
    Fst _  e     -> FAV.fst  <$> cnv (e , r)
    Snd _  e     -> FAV.snd  <$> cnv (e , r)
    Len  _  e    -> FAV.len  <$> cnv (e , r)
    LenV _  e    -> FAV.lenV <$> cnv (e , r)
    LeT _  el eb -> FAV.leT  <$> cnv (el , r) <*> cnv (eb , r)
    Eql _  el eb -> FAV.eql  <$> cnv (el , r) <*> cnv (eb , r)
    Ltd _  el eb -> FAV.ltd  <$> cnv (el , r) <*> cnv (eb , r)
    May _ l m n  -> FAV.may  <$> cnv (l , r) <*> cnv (m , r) <*> cnv (n , r)
    Typ _ e      -> pure (cnv (e , r))
    _ -> $(biGenOverloadedML 'ee ''Exp "FAV"
     ['Prm,'Var,'App,'Fst,'Snd,'Len,'LenV,'LeT,'Eql,'Ltd,'May,'Typ]
      (const [| \ e -> cnv (e , r) |])))

instance Cnv (Exp m (Suc n) a , (Env m FAV.Exp , Env n FAV.Exp)) (FAV.Exp -> FAV.Exp) where
  cnv (e , (s , g)) = pure (\ x -> frmRgtZro (cnv (e , (s , Ext x g))))
