module QFeldspar.Expression.Feldspar.MiniWellScoped
    (Exp(..),sucAll,prdAll,mapVar,isFresh,absVar,absTmp,eql,eqlF
    ,cntTmp,hasOneOrZro) where

import QFeldspar.MyPrelude hiding (foldl)

import qualified QFeldspar.Type.Feldspar.GADT     as TFG

import QFeldspar.Variable.Typed

import QFeldspar.Environment.Typed as ET
import QFeldspar.Singleton

import Data.IORef
import System.IO.Unsafe


data Exp :: [*] -> * -> * where
  ConI  :: Int      -> Exp r Int
  ConB  :: Bool     -> Exp r Bol
  ConF  :: Float    -> Exp r Flt
  AppV  :: HasSin TFG.Typ t =>
           Var r t  -> Env (Exp r) (TFG.Arg t) -> Exp r (TFG.Out t)
  Cnd   :: Exp r Bol -> Exp r t -> Exp r t -> Exp r t
  Whl   :: (Exp r t -> Exp r Bol) -> (Exp r t -> Exp r t) ->
           Exp r t  -> Exp r t
  Tpl   :: Exp r tf -> Exp r ts -> Exp r (Tpl tf ts)
  Fst   :: HasSin TFG.Typ ts =>
           Exp r (Tpl tf ts)-> Exp r tf
  Snd   :: HasSin TFG.Typ tf =>
           Exp r (Tpl tf ts)-> Exp r ts
  Ary   :: Exp r Int -> (Exp r Int -> Exp r t) -> Exp r (Ary t)
  Len   :: HasSin TFG.Typ ta =>
           Exp r (Ary ta) -> Exp r Int
  Ind   :: Exp r (Ary ta) -> Exp r Int -> Exp r ta
  Let   :: HasSin TFG.Typ tl =>
           Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb
  Cmx   :: Exp r Flt -> Exp r Flt -> Exp r Cmx
  Tmp   :: String -> Exp r t  -- dummy constructor
  Tag   :: String -> Exp r t -> Exp r t
  Non   :: Exp r (May tl)
  Som   :: Exp r tl -> Exp r (May tl)
  May   :: HasSin TFG.Typ a =>
           Exp r (May a) -> Exp r b -> (Exp r a -> Exp r b) -> Exp r b

deriving instance Show (Exp r t)

ref :: IORef Int
{-# NOINLINE ref #-}
ref = unsafePerformIO (newIORef 0)

instance Show (Exp r ta -> Exp r tb) where
  show f =

    let i = unsafePerformIO (do j <- readIORef ref
                                modifyIORef ref (+1)
                                return j)
        v = ("_x" ++ show i)
    in ("(\\ "++ v ++ " -> (" ++
        show (f (Tmp v))
        ++ "))")

instance Show (Env (Exp r) r') where
  show Emp        = "[]"
  show (Ext x xs) = "(" ++ show x ++ ") , " ++ show xs

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
eql Non         Non           = True
eql (Som e)     (Som e')      = eql e e'
eql (May (em  :: Exp r (May tm)) en  es)
    (May (em' :: Exp r (May tm')) en' es') =
  case eqlSin (sin :: TFG.Typ tm)(sin :: TFG.Typ tm') of
    Rgt Rfl -> eql em em' && eql en en' && eqlF es es'
    _       -> False
eql _           _             = False

eqlF :: forall r ta tb.  (Exp r ta -> Exp r tb) -> (Exp r ta -> Exp r tb) -> Bool
eqlF f f' = let v = genNewNam
            in eql (f (Tmp v)) (f' (Tmp v))

sucAll :: Exp r t' -> Exp (t ': r) t'
sucAll = mapVar Suc prd

prdAll :: Exp (t ': r) t' -> Exp r t'
prdAll = mapVar prd Suc

mapVar :: (forall t'. Var r  t' -> Var r' t') ->
          (forall t'. Var r' t' -> Var r  t') ->
          Exp r t -> Exp r' t
mapVar _ _ (ConI i)       = ConI i
mapVar _ _ (ConB i)       = ConB i
mapVar _ _ (ConF i)       = ConF i
mapVar f g (AppV v es)    = AppV (f v) (ET.fmap (mapVar f g) es)
mapVar f g (Cnd ec et ef) = Cnd (mapVar f g ec) (mapVar f g et) (mapVar f g ef)
mapVar f g (Whl ec eb ei) = Whl (mapVar f g . ec . mapVar g f)
                                (mapVar f g . eb . mapVar g f) (mapVar f g ei)
mapVar f g (Tpl ef es)    = Tpl (mapVar f g ef) (mapVar f g es)
mapVar f g (Fst e)        = Fst (mapVar f g e)
mapVar f g (Snd e)        = Snd (mapVar f g e)
mapVar f g (Ary el ef)    = Ary (mapVar f g el) (mapVar f g . ef . mapVar g f)
mapVar f g (Len e)        = Len (mapVar f g e)
mapVar f g (Ind ea ei)    = Ind (mapVar f g ea) (mapVar f g ei)
mapVar f g (Let el eb)    = Let (mapVar f g el) (mapVar f g . eb . mapVar g f)
mapVar f g (Cmx er ei)    = Cmx (mapVar f g er) (mapVar f g ei)
mapVar _ _ (Tmp x)        = Tmp x
mapVar f g (Tag x e)      = Tag x (mapVar f g e)
mapVar _ _ Non            = Non
mapVar f g (Som e)        = Som (mapVar f g e)
mapVar f g (May em en es) = May (mapVar f g em) (mapVar f g en) (mapVarF f g es)

mapVarF :: (forall t'. Var r  t' -> Var r' t') ->
           (forall t'. Var r' t' -> Var r  t') ->
           (Exp r a -> Exp r b) -> (Exp r' a -> Exp r' b)
mapVarF f g ff = mapVar f g . ff . mapVar g f

absTmp :: forall r t t'. (HasSin TFG.Typ t', HasSin TFG.Typ t) =>
          Exp r t' -> String -> Exp r t -> Exp r t
absTmp xx s ee = let t = sin :: TFG.Typ t in case ee of
  ConI i                    -> ConI i
  ConB i                    -> ConB i
  ConF i                    -> ConF i
  AppV v es                 -> AppV v (TFG.mapC (sinTyp v) (absTmp xx s) es)
  Cnd ec et ef              -> Cnd (absTmp xx s ec)   (absTmp xx s et)
                                    (absTmp xx s ef)
  Whl ec eb ei              -> Whl (absTmp xx s . ec) (absTmp xx s . eb)
                                   (absTmp xx s ei)
  Tpl ef es                 -> case TFG.getPrfHasSinTpl t of
    (PrfHasSin , PrfHasSin) -> Tpl (absTmp xx s ef)   (absTmp xx s es)
  Fst e                     -> Fst (absTmp xx s e)
  Snd e                     -> Snd (absTmp xx s e)
  Ary el ef                 -> case TFG.getPrfHasSinAry t of
    PrfHasSin               -> Ary (absTmp xx s el)   (absTmp xx s . ef)
  Len e                     -> Len (absTmp xx s e)
  Ind ea ei                 -> Ind (absTmp xx s ea)   (absTmp xx s ei)
  Let el eb                 -> Let (absTmp xx s el)   (absTmp xx s . eb)
  Cmx er ei                 -> Cmx (absTmp xx s er)   (absTmp xx s ei)
  Tmp x
    | s == x                -> case eqlSin (sinTyp xx) t of
      Rgt Rfl               -> xx
      _                     -> ee
    | otherwise             -> ee
  Tag x e                   -> Tag x (absTmp xx s e)
  Non                       -> Non
  Som e                     -> case TFG.getPrfHasSinMay t of
   PrfHasSin                -> Som (absTmp xx s e)
  May ec en es              -> May (absTmp xx s ec) (absTmp xx s en)
                                   (absTmp xx s . es)

absVar :: forall r a b. (HasSin TFG.Typ a, HasSin TFG.Typ b) =>
         Exp (a ': r) b -> Exp r a -> Exp r b

absVar ee xx = absVar' xx ee


absVar' :: forall r a b. (HasSin TFG.Typ a, HasSin TFG.Typ b) =>
          Exp r a -> Exp (a ': r) b -> Exp r b
absVar' xx ee = let b = sin :: TFG.Typ b in case ee of
  ConI i                    -> ConI i
  ConB i                    -> ConB i
  ConF i                    -> ConF i
  AppV v@Zro _              -> case sinTyp v of
    TFG.Int                 -> xx
    TFG.Bol                 -> xx
    TFG.Flt                 -> xx
    TFG.Tpl _ _             -> xx
    TFG.Ary _               -> xx
    TFG.May _               -> xx
    TFG.Cmx                 -> xx
    TFG.Arr _ _             -> impossible
  AppV v   es               -> AppV (prd v) (TFG.mapC (sinTyp v) (absVar' xx) es)
  Cnd ec et ef              -> Cnd (absVar' xx ec)   (absVar' xx et)
                                    (absVar' xx ef)
  Whl ec eb ei              -> Whl (absVar'F xx ec) (absVar'F xx eb)
                                   (absVar' xx ei)
  Tpl ef es                 -> case TFG.getPrfHasSinTpl b of
    (PrfHasSin , PrfHasSin) -> Tpl (absVar' xx ef)   (absVar' xx es)
  Fst e                     -> Fst (absVar' xx e)
  Snd e                     -> Snd (absVar' xx e)
  Ary el ef                 -> case TFG.getPrfHasSinAry b of
    PrfHasSin               -> Ary (absVar' xx el)   (absVar'F xx ef)
  Len e                     -> Len (absVar' xx e)
  Ind ea ei                 -> Ind (absVar' xx ea)   (absVar' xx ei)
  Let el eb                 -> Let (absVar' xx el)   (absVar'F xx eb)
  Cmx er ei                 -> Cmx (absVar' xx er)   (absVar' xx ei)
  Tmp x                     -> Tmp x
  Tag x e                   -> Tag x (absVar' xx e)
  Non                       -> Non
  Som e                     -> case TFG.getPrfHasSinMay b of
   PrfHasSin                -> Som (absVar' xx e)
  May ec en es              -> May (absVar' xx ec) (absVar' xx en)
                                   (absVar'F xx es)

absVar'F :: forall r a b c.
            (HasSin TFG.Typ a, HasSin TFG.Typ b, HasSin TFG.Typ c) =>
           Exp r a -> (Exp (a ': r) b -> Exp (a ': r) c) ->
                      (Exp r b -> Exp r c)
absVar'F xx ef = let v = genNewNam
                 in (\ x -> absTmp x v (absVar' xx (ef (Tmp v))))

-- when input string is not "__dummy__"
hasTmp :: String -> Exp r t -> Bool
hasTmp s ee = case ee of
  ConI _                    -> False
  ConB _                    -> False
  ConF _                    -> False
  AppV _ es                 -> foldl (\ b e -> b || hasTmp s e) False es
  Cnd ec et ef              -> hasTmp  s ec || hasTmp  s et || hasTmp  s ef
  Whl ec eb ei              -> hasTmpF s ec || hasTmpF s eb || hasTmp  s ei
  Tpl ef es                 -> hasTmp  s ef || hasTmp  s es
  Fst e                     -> hasTmp  s e
  Snd e                     -> hasTmp  s e
  Ary el ef                 -> hasTmp  s el || hasTmpF s ef
  Len e                     -> hasTmp  s e
  Ind ea ei                 -> hasTmp  s ea || hasTmp  s ei
  Let el eb                 -> hasTmp  s el || hasTmpF s eb
  Cmx er ei                 -> hasTmp  s er || hasTmp  s ei
  Tmp x
    | s == x                -> True
    | otherwise             -> False
  Tag _ e                   -> hasTmp  s e
  Non                       -> False
  Som e                     -> hasTmp  s e
  May em en es              -> hasTmp  s em || hasTmp  s en || hasTmpF  s es

hasTmpF :: String -> (Exp r ta -> Exp r tb) -> Bool
hasTmpF s f = let n = genNewNam
              in  hasTmp s (f (Tmp n))

-- when input string is not "__dummy__"
cntTmp :: String -> Exp r t -> Int
cntTmp s ee = case ee of
  ConI _                    -> 0
  ConB _                    -> 0
  ConF _                    -> 0
  AppV _ es                 -> foldl (\ b e -> b + cntTmp s e) 0 es
  Cnd ec et ef              -> cntTmp  s ec + cntTmp  s et + cntTmp  s ef
  Whl ec eb ei              -> cntTmpF s ec + cntTmpF s eb + cntTmp  s ei
  Tpl ef es                 -> cntTmp  s ef + cntTmp  s es
  Fst e                     -> cntTmp  s e
  Snd e                     -> cntTmp  s e
  Ary el ef                 -> cntTmp  s el + cntTmpF s ef
  Len e                     -> cntTmp  s e
  Ind ea ei                 -> cntTmp  s ea + cntTmp  s ei
  Let el eb                 -> cntTmp  s el + cntTmpF s eb
  Cmx er ei                 -> cntTmp  s er + cntTmp  s ei
  Tmp x
    | s == x                -> 1
    | otherwise             -> 0
  Tag _ e                   -> cntTmp  s e
  Non                       -> 0
  Som e                     -> cntTmp  s e
  May em en es              -> cntTmp  s em + cntTmp  s en + cntTmpF  s es

cntTmpF :: String -> (Exp r ta -> Exp r tb) -> Int
cntTmpF s f = let n = genNewNam
              in  cntTmp s (f (Tmp n))

hasOneOrZro :: (Exp r ta -> Exp r tb) -> Bool
hasOneOrZro f = let v = genNewNam
                in  cntTmp v (f (Tmp v)) <= 1

isFresh :: (Exp r ta -> Exp r tb) -> Bool
isFresh f = let n = genNewNam
            in  not (hasTmp n (f (Tmp n)))