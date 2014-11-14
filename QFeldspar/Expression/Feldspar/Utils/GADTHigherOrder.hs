{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Feldspar.Utils.GADTHigherOrder where

import QFeldspar.MyPrelude
import QFeldspar.Expression.Feldspar.Utils.Common
import QFeldspar.Expression.Feldspar.GADTHigherOrder
import QFeldspar.Variable.Typed
import qualified QFeldspar.Type.Feldspar.GADT as TFG
import QFeldspar.Singleton

eql :: forall r t .  Exp r t -> Exp r t -> Bool
eql (ConI i)    (ConI i')     = i == i'
eql (ConB b)    (ConB b')     = b == b'
eql (ConF f)    (ConF f')     = f == f'
eql (Var  v)    (Var  v')     = v == v'
eql (Abs  f)    (Abs  f')     = eqlF f f'
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
    Rgt Rfl -> eql el el' && eqlF eb eb'
    _       -> False
eql (Cmx ei er) (Cmx ei' er') = eql ei ei' && eql er er'
eql (Tmp x    ) (Tmp x')      = x == x'
eql Non         Non           = True
eql (Som e)     (Som e')      = eql e e'
eql (May (em  :: Exp r (May tm)) en  es)
    (May (em' :: Exp r (May tm')) en' es') =
  case eqlSin (sin :: TFG.Typ tm)(sin :: TFG.Typ tm') of
    Rgt Rfl -> eql em em' && eql en en' && eql es es'
    _       -> False
eql (Mul ei er) (Mul ei' er') = eql ei ei' && eql er er'
eql _           _             = False

eqlF :: forall r ta tb.  (Exp r ta -> Exp r tb) ->
        (Exp r ta -> Exp r tb) -> Bool
eqlF f f' = let v = genNewNam "__eqlFHO__"
                {-# NOINLINE v #-}
            in deepseq v $ eql (f (Tmp v)) (f' (Tmp v))

sucAll :: Exp r t' -> Exp (t ': r) t'
sucAll = mapVar Suc prd

prdAll :: Exp (t ': r) t' -> Exp r t'
prdAll = mapVar prd Suc

mapVar :: forall t r r'.
          (forall t'. Var r  t' -> Var r' t') ->
          (forall t'. Var r' t' -> Var r  t') ->
          Exp r t -> Exp r' t
mapVar f g ee = case ee of
  Var v -> Var (f v)
  _     -> $(genOverloaded 'ee ''Exp ['Var]
   (\ t -> if
    | matchQ t [t| Exp t t -> Exp t t |] ->
        [| \ ff -> mapVar f g . ff . mapVar g f |]
    | matchQ t [t| Exp t t |]          ->
        [| mapVar f g |]
    | otherwise                        ->
        [| id |]))

absTmp :: forall r t t'. (HasSin TFG.Typ t', HasSin TFG.Typ t) =>
          Exp r t' -> String -> Exp r t -> Exp r t
absTmp xx s ee = let t = sin :: TFG.Typ t in case ee of
  Tmp x
    | s == x    -> case eqlSin (sinTyp xx) (sin :: TFG.Typ t) of
      Rgt Rfl   -> xx
      _         -> ee
    | otherwise -> ee
  _             -> $(genOverloadedW 'ee ''Exp  ['Tmp] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| Exp t t -> Exp t t |] -> [| (absTmp xx s .) |]
    | matchQ tt [t| Exp t t |]            -> [| absTmp xx s |]
    | otherwise                           -> [| id |]))

hasTmp :: forall r t. String -> Exp r t -> Bool
hasTmp s ee = case ee of
  Tmp x
    | s == x    -> True
    | otherwise -> False
  _             -> $(recAppMQ 'ee ''Exp (const [| False |]) ['Tmp]
    [| \ _x -> False |] [| (||) |] [| (||) |] (const id)
   (\ tt -> if
    | matchQ tt [t| Exp t t -> Exp t t |] -> [| hasTmpF s |]
    | matchQ tt [t| Exp t t |]            -> [| hasTmp s |]
    | otherwise                           -> [| const False |]))

hasTmpF :: String -> (Exp r ta -> Exp r tb) -> Bool
hasTmpF s f = let v = genNewNam "hasTmpFGHO"
                  {-# NOINLINE v #-}
              in  deepseq v $ hasTmp s (f (Tmp v))

isFresh :: (Exp r ta -> Exp r tb) -> Bool
isFresh f = let v = genNewNam "isFreshGHO"
                {-# NOINLINE v #-}
            in  deepseq v $ not (hasTmp v (f (Tmp v)))
