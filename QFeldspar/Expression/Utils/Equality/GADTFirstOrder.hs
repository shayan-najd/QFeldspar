module QFeldspar.Expression.Utils.Equality.GADTFirstOrder (eql) where

import QFeldspar.MyPrelude

import QFeldspar.Expression.GADTFirstOrder
import QFeldspar.Singleton
import qualified QFeldspar.Type.GADT as TFG

eql :: forall r t .  Exp r t -> Exp r t -> Bool
eql (ConI i)    (ConI i')     = i == i'
eql (ConB b)    (ConB b')     = b == b'
eql (ConF f)    (ConF f')     = f == f'
eql (Var  v)    (Var  v')     = v == v'
eql (Abs  f)    (Abs  f')     = eql f f'
eql (App ef (ea :: Exp r ta)) (App ef' (ea' :: Exp r ta')) =
  case eqlSin (sin :: TFG.Typ ta) (sin :: TFG.Typ ta') of
    Rgt Rfl -> eql ef ef' && eql ea ea'
    _       -> False
eql (Cnd ec et ef) (Cnd ec' et' ef') = eql ec ec' && eql et et'
                                       && eql ef ef'
eql (Whl ec eb ei) (Whl ec' eb' ei') = eql ec ec' && eql eb eb'
                                       && eql ei ei'
eql (Tpl ef es)    (Tpl ef' es')     = eql ef ef' && eql es es'
eql (Fst (e :: Exp r (Tpl t ts))) (Fst (e' :: Exp r (Tpl t ts'))) =
  case eqlSin (sin :: TFG.Typ ts) (sin :: TFG.Typ ts') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Snd (e :: Exp r (Tpl tf t))) (Snd (e' :: Exp r (Tpl tf' t))) =
  case eqlSin (sin :: TFG.Typ tf) (sin :: TFG.Typ tf') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Ary ei ef) (Ary ei' ef') = eql ei ei' && eql ef ef'
eql (Len (e :: Exp r (Ary ta))) (Len (e' :: Exp r (Ary ta'))) =
  case eqlSin (sin :: TFG.Typ ta) (sin :: TFG.Typ ta') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Ind (e :: Exp r (Ary t)) ei) (Ind (e' :: Exp r (Ary t)) ei') =
    eql e e' && eql ei ei'
eql (AryV ei ef) (AryV ei' ef') = eql ei ei' && eql ef ef'
eql (LenV (e :: Exp r (Vec ta))) (LenV (e' :: Exp r (Vec ta'))) =
  case eqlSin (sin :: TFG.Typ ta) (sin :: TFG.Typ ta') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (IndV (e :: Exp r (Vec t)) ei) (IndV (e' :: Exp r (Vec t)) ei') =
    eql e e' && eql ei ei'
eql (Let (el :: Exp r ta) eb) (Let (el' :: Exp r ta') eb') =
  case eqlSin (sin :: TFG.Typ ta) (sin :: TFG.Typ ta') of
    Rgt Rfl -> eql el el' && eql eb eb'
    _       -> False
eql (Cmx ei er) (Cmx ei' er') = eql ei ei' && eql er er'
eql Non         Non           = True
eql (Som e)     (Som e')      = eql e e'
eql (May (em  :: Exp r (May tm )) en  es )
    (May (em' :: Exp r (May tm')) en' es') =
  case eqlSin (sin :: TFG.Typ tm) (sin :: TFG.Typ tm') of
    Rgt Rfl -> eql em em' && eql en en' && eql es es'
    _       -> False
eql (Mul ei er) (Mul ei' er') = eql ei ei' && eql er er'
eql (Add ei er) (Add ei' er') = eql ei ei' && eql er er'
eql (Int i)     (Int j)       = i == j
eql (Tag _ e)   (Tag _ e')    = eql e e' -- ignore tags
eql _           _             = False
