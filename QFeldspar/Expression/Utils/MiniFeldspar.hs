{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Utils.MiniFeldspar where

import QFeldspar.MyPrelude hiding (foldl)
import QFeldspar.Expression.Utils.Common
import QFeldspar.Expression.MiniFeldspar
import QFeldspar.Variable.Typed
import QFeldspar.Environment.Typed as ET hiding (fmap)
import qualified QFeldspar.Environment.Typed as ET
import qualified QFeldspar.Type.GADT as TFG
import QFeldspar.Singleton
import qualified Language.Haskell.TH as TH

tagFree :: Exp r t -> Exp r t
tagFree (Tag _ e) = tagFree e
tagFree e         = e

pattern TF e <- (tagFree -> e)

eqlE :: Env (Exp r) r' -> Env (Exp r) r' -> Bool
eqlE Emp        Emp        = True
eqlE (Ext x xs) (Ext y ys) = eql x y && eqlE xs ys
eqlE _          _          = False

eql :: forall r t .  Exp r t -> Exp r t -> Bool
eql (ConI i)    (ConI i')     = i == i'
eql (ConB b)    (ConB b')     = b == b'
eql (ConF f)    (ConF f')     = f == f'
eql (AppV (v :: Var r tv) es)    (AppV (v' :: Var r tv') es') =
  case eqlSin (sin :: TFG.Typ tv) (sin :: TFG.Typ tv') of
    Rgt Rfl -> v == v' && eqlE es es'
    _       -> False
eql (Cnd ec et ef) (Cnd ec' et' ef') = eql ec ec' && eql et et' && eql ef ef'
eql (Whl ec eb ei) (Whl ec' eb' ei') = eqlF ec ec' && eqlF eb eb' && eql ei ei'
eql (Tpl ef es)    (Tpl ef' es')     = eql ef ef' && eql es es'
eql (Fst (e :: Exp r (Tpl t ts))) (Fst (e' :: Exp r (Tpl t ts'))) =
  case eqlSin (sin :: TFG.Typ ts) (sin :: TFG.Typ ts') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Snd (e :: Exp r (Tpl tf t))) (Snd (e' :: Exp r (Tpl tf' t))) =
  case eqlSin (sin :: TFG.Typ tf) (sin :: TFG.Typ tf') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Ary ei ef) (Ary ei' ef') = eql ei ei' && eqlF ef ef'
eql (Len (e :: Exp r (Ary ta))) (Len (e' :: Exp r (Ary ta'))) =
  case eqlSin (sin :: TFG.Typ ta) (sin :: TFG.Typ ta') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Ind (e :: Exp r (Ary t)) ei) (Ind (e' :: Exp r (Ary t)) ei') =
    eql e e' && eql ei ei'
eql (Let (el :: Exp r ta) eb) (Let (el' :: Exp r ta') eb') =
  case eqlSin (sin :: TFG.Typ ta) (sin :: TFG.Typ ta') of
    Rgt Rfl -> eql el el' && eqlF eb eb'
    _       -> False
eql (Cmx ei er) (Cmx ei' er') = eql ei ei' && eql er er'
eql (Tmp x    ) (Tmp x')      = x == x'
eql (Tag _ e)   e'            = eql e e'
eql e          (Tag _ e')     = eql e e'
eql (Mul ei er) (Mul ei' er') = eql ei ei' && eql er er'
eql _           _             = False

eqlF :: forall r ta tb.
        (Exp r ta -> Exp r tb) -> (Exp r ta -> Exp r tb) -> Bool
eqlF f f' = let v = genNewNam "__eqlFMS__"
                {-# NOINLINE v #-}
            in deepseq v $ eql (f (Tmp v)) (f' (Tmp v))

sucAll :: Exp r t' -> Exp (t ': r) t'
sucAll = mapVar Suc prd

prdAll :: Exp (t ': r) t' -> Exp r t'
prdAll = mapVar prd Suc

mapVar :: forall r r' t.
          (forall t'. Var r  t' -> Var r' t') ->
          (forall t'. Var r' t' -> Var r  t') ->
          Exp r t -> Exp r' t
mapVar f g ee = case ee of
  AppV v es -> AppV (f v) (ET.fmap (mapVar f g) es)
  _         ->  $(genOverloaded 'ee ''Exp ['AppV]
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
  AppV v es     -> AppV v (TFG.mapC (sinTyp v) (absTmp xx s) es)
  Tmp x
    | s == x    -> case eqlSin (sinTyp xx) (sin :: TFG.Typ t) of
      Rgt Rfl   -> xx
      _         -> ee
    | otherwise -> ee
  _             -> $(genOverloadedW 'ee ''Exp  ['AppV,'Tmp] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| Exp t t -> Exp t t |] -> [| (absTmp xx s .) |]
    | matchQ tt [t| Exp t t |]            -> [| absTmp xx s |]
    | otherwise                           -> [| id |]))

absVar :: forall r a b. (HasSin TFG.Typ a, HasSin TFG.Typ b) =>
         Exp (a ': r) b -> Exp r a -> Exp r b
absVar ee xx = absVar' xx ee

absVar' :: forall r a b. (HasSin TFG.Typ a, HasSin TFG.Typ b) =>
          Exp r a -> Exp (a ': r) b -> Exp r b
absVar' xx ee = let b = sin :: TFG.Typ b in case ee of
  AppV v@Zro _              -> case sinTyp v of
    TFG.Int                 -> xx
    TFG.Bol                 -> xx
    TFG.Flt                 -> xx
    TFG.Tpl _ _             -> xx
    TFG.Ary _               -> xx
    TFG.Vct _               -> xx
    TFG.May _               -> xx
    TFG.Cmx                 -> xx
    TFG.Arr _ _             -> impossible
  AppV v   es               -> AppV (prd v) (TFG.mapC
                                  (sinTyp v) (absVar' xx) es)
  _  -> $(genOverloadedW 'ee ''Exp  ['AppV] (trvWrp 'b)
   (\ tt -> if
    | matchQ tt [t| Exp a a -> Exp a a |] -> [| absVar'F xx |]
    | matchQ tt [t| Exp a a |]            -> [| absVar'  xx |]
    | otherwise                           -> [| id |]))

absVar'F :: forall r a b c.
            (HasSin TFG.Typ a, HasSin TFG.Typ b, HasSin TFG.Typ c) =>
           Exp r a -> (Exp (a ': r) b -> Exp (a ': r) c) ->
                      (Exp r b -> Exp r c)
absVar'F xx ef = let v = genNewNam "absVar'F"
                     {-# NOINLINE v #-}
                 in deepseq v $ (\ x -> absTmp x v
                                        (absVar' xx (ef (Tmp v))))

hasTmp :: String -> Exp r t -> Bool
hasTmp s ee = cntTmp s ee > 0

cntTmp :: forall r t. String -> Exp r t -> Int
cntTmp s ee = case ee of
  AppV _ es     -> foldl (\ b e -> b + cntTmp s e) 0 es
  Tmp x
    | s == x    -> 1
    | otherwise -> 0
  _             -> $(recAppMQ 'ee ''Exp (const [| 0 |]) ['AppV,'Tmp]
    [| \ _x -> 0 |] [| (+) |] [| (+) |] (const id)
   (\ tt -> if
    | matchQ tt [t| Exp t t -> Exp t t |] -> [| cntTmpF s |]
    | matchQ tt [t| Exp t t |]            -> [| cntTmp  s |]
    | otherwise                           -> [| const 0   |]))


cntTmpF :: String -> (Exp r ta -> Exp r tb) -> Int
cntTmpF s f = let v = genNewNam "cntTmpF"
                  {-# NOINLINE v #-}
              in  deepseq v $ cntTmp s (f (Tmp v))

hasOneOrZro :: (Exp r ta -> Exp r tb) -> Bool
hasOneOrZro f = let v = genNewNam "hasOneOrZro"
                    {-# NOINLINE v #-}
                in  deepseq v $ cntTmp v (f (Tmp v)) <= 1

isFresh :: (Exp r ta -> Exp r tb) -> Bool
isFresh f = let v = genNewNam "isFreshMWS"
                {-# NOINLINE v #-}
            in  deepseq v $ not (hasTmp v (f (Tmp v)))

remTag :: forall r t. Exp r t -> Exp r t
remTag ee = case ee of
  AppV v es -> AppV v (TFG.mapC (sinTyp v) remTag es)
  Tag _  e  -> remTag e
  _         -> $(genOverloaded 'ee ''Exp  ['AppV,'Tag]
   (\ tt -> if
    | matchQ tt [t| Exp t t -> Exp t t |] -> [| remTagF |]
    | matchQ tt [t| Exp t t |]            -> [| remTag  |]
    | otherwise                           -> [| id |]))

remTagF :: (Exp r a -> Exp r b) -> (Exp r a -> Exp r b)
remTagF = (remTag .)

shared :: TH.Q TH.Exp
shared = [| Tag $(do s <- fmap show (TH.newName "v")
                     [|s|]) |]
