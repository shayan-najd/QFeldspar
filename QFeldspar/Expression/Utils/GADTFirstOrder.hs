{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Utils.GADTFirstOrder
       (sucAll,prdAll,prdAllM,mapVar,sbs,replaceOne,cntVar,eql,pattern TF) where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTFirstOrder as FGFO
import QFeldspar.Variable.Typed
import QFeldspar.Expression.Utils.Common
import QFeldspar.Singleton
import QFeldspar.Type.GADT hiding (Int,May,Cmx,Ary,Tpl)

tagFree :: Exp r t -> Exp r t
tagFree (Tag _ e) = tagFree e
tagFree e         = e

pattern TF e <- (tagFree -> e)

sucAll :: Exp r t' -> Exp (t ': r) t'
sucAll = mapVar Suc

prdAll :: Exp (t ': r) t' -> Exp r t'
prdAll = mapVar (\(Suc x) -> x)

prdAllM :: Exp (t ': r) t' -> Maybe (Exp r t')
prdAllM = mapVarM (\ v -> case v of
                            Zro -> Nothing
                            Suc x -> Just x)


mapVarM :: forall r r' t m. (Applicative m , Monad m) =>
          (forall t'. Var r t' -> m (Var r' t')) -> Exp r t -> m (Exp r' t)
mapVarM f ee = case ee of
 Var v -> Var <$> f v
 _     -> $(genOverloadedM 'ee ''Exp  ['Var]
  (\ t -> if
      | matchQ t [t| Exp (t ': t) t |] -> [| mapVarM (incM f) |]
      | matchQ t [t| Exp t t |]        -> [| mapVarM f  |]
      | otherwise                      -> [| pure |]))


mapVar :: forall r r' t.
          (forall t'. Var r t' -> Var r' t') -> Exp r t -> Exp r' t
mapVar f ee = case ee of
 Var v -> Var (f v)
 _     -> $(genOverloaded 'ee ''Exp  ['Var]
  (\ t -> if
      | matchQ t [t| Exp (t ': t) t |] -> [| mapVar (inc f) |]
      | matchQ t [t| Exp t t |]        -> [| mapVar f  |]
      | otherwise                      -> [| id |]))

deriving instance Show (Exp g a)

sbs :: (HasSin Typ a, HasSin Typ b) =>
       Exp r a -> Exp (a ': r) b -> Exp r b
sbs e' ee = prdAll (sbs' (sucAll e') Zro ee)

rp :: Var (a ': g) t -> Var (a ': b ': g) t
rp v = case v of
         Zro    -> Zro
         Suc v' -> Suc (Suc v')

replaceOne :: Exp (tl ': n) t -> Exp (tl ': tl1 ': n) t
replaceOne = mapVar rp

cntVar :: forall r t t'. (HasSin Typ t' , HasSin Typ t) =>
          Var r t' -> Exp r t -> Int
cntVar v ee = let t = sin :: Typ t in case ee of
  Var x     -> case eqlSin t (sinTyp v) of
    Rgt Rfl -> if x == v
               then 1
               else 0
    _       -> 0
  _         -> $(recAppMQ 'ee ''Exp (const [| (0 :: Int) |]) ['Var]
    [| \ _x -> 0 |] [| (+) |] [| (+) |] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| Exp (t ': t) t |]     -> [| cntVar (Suc v) |]
    | matchQ tt [t| Exp t t |]            -> [| cntVar v |]
    | otherwise                           -> [| const 0  |]))

sbs' :: forall r t t'.
       (HasSin Typ t , HasSin Typ t') =>
       Exp r t' -> Var r t' -> Exp r t -> Exp r t
sbs' e' v' ee = let t = sin :: Typ t in case ee of
  Var v -> case eqlSin t (sinTyp v') of
     Rgt Rfl -> if v == v'
                then e'
                else ee
     _       -> ee
  _   -> $(genOverloadedW 'ee ''Exp  ['Var] (trvWrp 't)
   (\ tt -> if
      | matchQ tt [t| Exp (t ': t) t |] -> [| sbs'F e' v' |]
      | matchQ tt [t| Exp t t |]        -> [| sbs'  e' v' |]
      | otherwise                       -> [| id |]))

sbs'F :: forall r t t' t''.
        (HasSin Typ t', HasSin Typ t) =>
        Exp r t' -> Var r t' -> Exp (t'' ': r) t -> Exp (t'' ': r) t
sbs'F e' v' e = sbs' (sucAll e') (Suc v') e

eql :: forall r t .  Exp r t -> Exp r t -> Bool
eql (ConI i)    (ConI i')     = i == i'
eql (ConB b)    (ConB b')     = b == b'
eql (ConF f)    (ConF f')     = f == f'
eql (Var  v)    (Var  v')     = v == v'
eql (Abs  f)    (Abs  f')     = eql f f'
eql (App ef (ea :: Exp r ta)) (App ef' (ea' :: Exp r ta')) =
  case eqlSin (sin :: Typ ta) (sin :: Typ ta') of
    Rgt Rfl -> eql ef ef' && eql ea ea'
    _       -> False
eql (Cnd ec et ef) (Cnd ec' et' ef') = eql ec ec' && eql et et'
                                       && eql ef ef'
eql (Whl ec eb ei) (Whl ec' eb' ei') = eql ec ec' && eql eb eb'
                                       && eql ei ei'
eql (Tpl ef es)    (Tpl ef' es')     = eql ef ef' && eql es es'
eql (Fst (e :: Exp r (Tpl t ts))) (Fst (e' :: Exp r (Tpl t ts'))) =
  case eqlSin (sin :: Typ ts) (sin :: Typ ts') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Snd (e :: Exp r (Tpl tf t))) (Snd (e' :: Exp r (Tpl tf' t))) =
  case eqlSin (sin :: Typ tf) (sin :: Typ tf') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Ary ei ef) (Ary ei' ef') = eql ei ei' && eql ef ef'
eql (Len (e :: Exp r (Ary ta))) (Len (e' :: Exp r (Ary ta'))) =
  case eqlSin (sin :: Typ ta) (sin :: Typ ta') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Ind (e :: Exp r (Ary t)) ei) (Ind (e' :: Exp r (Ary t)) ei') =
    eql e e' && eql ei ei'
eql (AryV ei ef) (AryV ei' ef') = eql ei ei' && eql ef ef'
eql (LenV (e :: Exp r (Vec ta))) (LenV (e' :: Exp r (Vec ta'))) =
  case eqlSin (sin :: Typ ta) (sin :: Typ ta') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (IndV (e :: Exp r (Vec t)) ei) (IndV (e' :: Exp r (Vec t)) ei') =
    eql e e' && eql ei ei'
eql (Let (el :: Exp r ta) eb) (Let (el' :: Exp r ta') eb') =
  case eqlSin (sin :: Typ ta) (sin :: Typ ta') of
    Rgt Rfl -> eql el el' && eql eb eb'
    _       -> False
eql (Cmx ei er) (Cmx ei' er') = eql ei ei' && eql er er'
eql Non         Non           = True
eql (Som e)     (Som e')      = eql e e'
eql (May (em  :: Exp r (May tm )) en  es )
    (May (em' :: Exp r (May tm')) en' es') =
  case eqlSin (sin :: Typ tm) (sin :: Typ tm') of
    Rgt Rfl -> eql em em' && eql en en' && eql es es'
    _       -> False
eql (Mul ei er) (Mul ei' er') = eql ei ei' && eql er er'
eql (Int i)     (Int j)       = i == j
eql _           _             = False
