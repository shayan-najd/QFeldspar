module QFeldspar.Expression.Utils.Equality.GADTFirstOrder (eql) where

import QFeldspar.MyPrelude

import QFeldspar.Variable.Typed
import QFeldspar.Environment.Typed
import QFeldspar.Expression.GADTFirstOrder
import QFeldspar.Singleton
import qualified QFeldspar.Type.GADT as TG

tt :: TG.Typ a
tt = tt

eql :: forall s g a.  Exp s g a -> Exp s g a -> Bool
eql (Lit i)     (Lit i')      = i == i'
eql (Lit _)     _             = False

eql (ConB b)    (ConB b')     = b == b'
eql (ConB _)    _             = False

eql (Var  v)    (Var  v')     = v == v'
eql (Var  _)    _             = False

eql (Prm x  (ns  :: Env (Exp s g) d))
    (Prm x' (ns' :: Env (Exp s g) d')) = case eqlSin (sinTyp ns  :: Env TG.Typ d)
                                                     (sinTyp ns' :: Env TG.Typ d') of
   Rgt Rfl -> eqlVar x x' && eqlEnv ns ns'
   Lft _   -> False
eql (Prm _ _)   _             = False

eql (Abs  f)    (Abs  f')     = eql f f'
eql (Abs  _)    _             = False

eql (App ef ea) (App ef' ea') =
  case eqlSin (sinTypOf ea tt) (sinTypOf ea' tt) of
    Rgt Rfl -> eql ef ef' && eql ea ea'
    _       -> False
eql (App _ _)   _             = False

eql (Cnd ec et ef) (Cnd ec' et' ef') = eql ec ec' && eql et et'
                                       && eql ef ef'
eql (Cnd _  _  _ ) _                 = False

eql (Whl ec eb ei) (Whl ec' eb' ei') = eql ec ec' && eql eb eb'
                                       && eql ei ei'
eql (Whl _  _  _ ) _                 = False

eql (Tpl ef es)    (Tpl ef' es')     = eql ef ef' && eql es es'
eql (Tpl _  _ )    _                 = False

eql (Fst (e :: Exp s g (t , ts))) (Fst (e' :: Exp s g (t  , ts'))) =
  case eqlSin (sin :: TG.Typ ts) (sin :: TG.Typ ts') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Fst _)     _               = False

eql (Snd (e :: Exp s g (tf , t))) (Snd (e' :: Exp s g (tf' , t))) =
  case eqlSin (sin :: TG.Typ tf) (sin :: TG.Typ tf') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Snd _)     _               = False

eql (Ary ei ef) (Ary ei' ef')   = eql ei ei' && eql ef ef'
eql (Ary _  _)  _               = False

eql (Len e) (Len e') =
  case eqlSin (sinTypOf e tt) (sinTypOf e' tt) of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Len _)     _               = False

eql (Ind e ei) (Ind e' ei')     = eql e e' && eql ei ei'
eql (Ind _ _)    _              = False

eql (AryV ei ef) (AryV ei' ef') = eql ei ei' && eql ef ef'
eql (AryV _ _)   _              = False

eql (LenV e) (LenV e') =
  case eqlSin (sinTypOf e tt) (sinTypOf e' tt) of
    Rgt Rfl -> eql e e'
    _       -> False
eql (LenV _)   _              = False

eql (IndV e ei) (IndV e' ei') = eql e e' && eql ei ei'
eql (IndV _ _) _              = False

eql (LeT el eb) (LeT el' eb') =
  case eqlSin (sinTypOf el tt) (sinTypOf el' tt) of
    Rgt Rfl -> eql el el' && eql eb eb'
    _       -> False
eql (LeT _ _)   _             = False

eql (Cmx ei er) (Cmx ei' er') = eql ei ei' && eql er er'
eql (Cmx _  _)  _             = False

eql Non         Non           = True
eql Non         _             = False

eql (Som e)     (Som e')      = eql e e'
eql (Som _)     _             = False

eql (May em  en  es ) (May em' en' es') =
  case eqlSin (sinTypOf em tt) (sinTypOf em' tt) of
    Rgt Rfl -> eql em em' && eql en en' && eql es es'
    _       -> False
eql (May _ _ _) _             = False

eql (Mul ei er) (Mul ei' er') = eql ei ei' && eql er er'
eql (Mul _  _ ) _             = False

eql (Add ei er) (Add ei' er') = eql ei ei' && eql er er'
eql (Add _  _ ) _             = False

eql (Sub ei er) (Sub ei' er') = eql ei ei' && eql er er'
eql (Sub _  _ ) _             = False

eql (Eql ei er) (Eql ei' er') = case eqlSin (sinTypOf ei tt) (sinTyp ei') of
    Rgt Rfl -> eql ei ei' && eql er er'
    _       -> False
eql (Eql _  _ ) _             = False

eql (Ltd ei er) (Ltd ei' er') = case eqlSin (sinTypOf ei tt) (sinTyp ei') of
    Rgt Rfl -> eql ei ei' && eql er er'
    _       -> False
eql (Ltd _  _ ) _             = False

eql (Int i)     (Int j)       = i == j
eql (Int _)     _             = False

eql (Rat i)     (Rat j)       = i == j
eql (Rat _)     _             = False

eql (Tag _ e)   (Tag _ e')    = eql e e' -- ignore tags
eql (Tag _ _)   _             = False

eql (Mem e)     (Mem e')      = eql e e'
eql (Mem _)     _             = False

eql (Fix e)     (Fix e')      = eql e e'
eql (Fix _)     _             = False

eqlEnv :: forall d d' s g. (TG.Types d , TG.Types d') =>
          Env (Exp s g) d ->  Env (Exp s g) d' -> Bool
eqlEnv d d' = case eqlSin (sin :: Env TG.Typ d) (sin :: Env TG.Typ d') of
   Rgt Rfl -> case (d , d') of
     (Emp      , Emp)        -> True
     (Ext x xs , Ext x' xs') -> case (TG.getPrfHasSinEnvOf d , TG.getPrfHasSinEnvOf d') of
       ((PrfHasSin , PrfHasSin),(PrfHasSin , PrfHasSin)) -> eql x x' && eqlEnv xs xs'
     _                       -> False
   _                         -> False
