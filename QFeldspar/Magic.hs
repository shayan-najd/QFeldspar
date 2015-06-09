-- It is safe to add the following for this module
{-# LANGUAGE UndecidableInstances #-}

module QFeldspar.Magic where

import QFeldspar.MyPrelude hiding (lookup)
import QFeldspar.Singleton
import QFeldspar.Nat.ADT hiding (add)
import qualified QFeldspar.Type.GADT as TG
import QFeldspar.Environment.Typed
import qualified QFeldspar.Nat.GADT as NG

-- Subtituition
type family Subs s a where
  Subs '[] a                              = a
  Subs s   (TVr x)                        = Case_Subs x (Lookup x s)
  -- Todo: add support for higher number of arguments
  Subs s   (k (a :: *) (b :: *) (c :: *)) = k (Subs s a) (Subs s b) (Subs s c)
  Subs s   (k (a :: *) (b :: *))          = k (Subs s a) (Subs s b)
  Subs s   (k (a :: *))                   = k (Subs s a)
  Subs s   a                              = a

-- Helper function
type family Case_Subs x t where
  Case_Subs x (Just t) = t
  Case_Subs x Nothing  = TVr x

-- Most general unifier
type family Mgu a a' :: Maybe [(Nat,*)] where
  Mgu (TVr x)   a'           = Just '[ '(x  , a')]
  Mgu (k a b c) (k a' b' c') = Case_Mgu c c' (Case_Mgu b b' (Mgu a a'))
  Mgu (k a b)   (k a' b')    = Case_Mgu b b' (Mgu a a')
  Mgu (k a)     (k a')       = Mgu a a'
  Mgu a         a            = Just '[]
  Mgu a         a'           = Nothing

-- Helper Function
type family Case_Mgu b b' t  where
  Case_Mgu b b' (Just s) = FMapAppend s (Mgu (Subs s b) b')
  Case_Mgu b b' Nothing  = Nothing

-- type-level fmap (xs ++) mys
type family FMapAppend xs mys where
  FMapAppend '[] x         = x
  FMapAppend xs  (Just ys) = Just (Add xs ys)
  FMapAppend xs  Nothing   = Nothing

-- fully applies a function type to list of arguments
type family AppFul a b where
  AppFul (a -> b) (a' ': as) = Case_AppFul (Mgu a a') b as
  AppFul (a -> b) '[]        = Nothing
  AppFul a        '[]        = Just a
  AppFul a        a'         = Nothing

-- Helper Function
type family Case_AppFul s b as where
  Case_AppFul (Just s) b as = AppFul (Subs s b) as
  Case_AppFul Nothing  b as = Nothing

-- a predicate stating whether a type has TVr inside it
type family NoTVr a where
  NoTVr (TVr x)  = False
  NoTVr (k a b c) = And  (NoTVr a)
                     (And (NoTVr b)
                          (NoTVr c))
  NoTVr (k a b)   = And  (NoTVr a)
                          (NoTVr b)
  NoTVr (k a)     = NoTVr a
  NoTVr a         = True

type Match a d b = Case_Match (AppFul a d) b ~ True

type family Case_Match c b where
  Case_Match (Just c) b = ToBool (Mgu c b)
  Case_Match Nothing  b = False

type family ToBool s where
  ToBool (Just x) = True
  ToBool Nothing  = False

type family NoTVrs as where
  NoTVrs '[]       = True
  NoTVrs (a ': as) = Case_NoTVrs (NoTVr a) as

type family Case_NoTVrs p as where
  Case_NoTVrs True  as = NoTVrs as
  Case_NoTVrs False as = False

data PrfNoTVr a where
  PrfNoTVr :: NoTVr a ~ True => PrfNoTVr a

data PrfNoTVrs as where
  PrfNoTVrs :: NoTVrs as ~ True => PrfNoTVrs as

getPrfNoTVrsEnv :: forall a as ass f.
                  (NoTVrs ass ~ True , ass ~ (a ': as)) =>
                  f ass -> (PrfNoTVr a , PrfNoTVrs as)
getPrfNoTVrsEnv _ = case obvious :: NoTVr a :~: 'True of
  Rfl -> (PrfNoTVr , PrfNoTVrs)


data PrfMatch a as b where
  PrfMatch :: Match a as b => PrfMatch a as b

getPrfMatch :: forall a d b m.
         Monad m =>
         TG.Typ a -> Env TG.Typ d -> TG.Typ b -> m (PrfMatch a d b)
getPrfMatch a d b = case case_Match (appFul a d) b of
  STrue -> return PrfMatch
  SFalse -> fail "Type Error when checking Match constraint!"

subs :: Env (SPair NG.Nat TG.Typ) s -> TG.Typ a -> TG.Typ (Subs s a)
subs Emp a          = a
subs s@(Ext _ _) (TG.TVr x)   = case_Subs x (lookup x s)
subs s@(Ext _ _) (TG.Arr a b) = TG.Arr (subs s a) (subs s b)
subs s@(Ext _ _) (TG.Tpl a b) = TG.Tpl (subs s a) (subs s b)
subs s@(Ext _ _) (TG.Ary a)   = TG.Ary (subs s a)
subs s@(Ext _ _) (TG.Vct a)   = TG.Vct (subs s a)
subs s@(Ext _ _) (TG.May a)   = TG.May (subs s a)
subs (Ext _ _)   TG.Wrd       = TG.Wrd
subs (Ext _ _)   TG.Bol       = TG.Bol
subs (Ext _ _)   TG.Flt       = TG.Flt
subs (Ext _ _)   TG.Cmx       = TG.Cmx

case_Subs :: NG.Nat x -> SMaybe TG.Typ a -> TG.Typ (Case_Subs x a)
case_Subs _ (SJust t) = t
case_Subs x SNothing  = TG.TVr x

lookup :: forall n xss f g. EqlSin f =>
          f n -> Env (SPair f g) xss ->
            SMaybe g (Lookup n xss)
lookup _ Emp = SNothing
lookup x (Ext (SPair (x' :: f x') (a :: g a))
               (xas :: Env (SPair f g) xas)) =
 case eqlSin x x' of
  Rgt Rfl -> SJust a
  Lft _   -> case obvious :: Lookup n ('( x' , a) ': xas) :~:
                             Lookup n xas  of
    Rfl   ->  lookup x xas

data SMaybe (f :: k -> *) (a :: Maybe k) where
  SNothing :: SMaybe f Nothing
  SJust :: f x -> SMaybe f (Just x)

data SPair f g a where
  SPair :: f a -> g b -> SPair f g '(a , b)

mgu :: forall a a'. TG.Typ a -> TG.Typ a' ->
       SMaybe (Env (SPair NG.Nat TG.Typ)) (Mgu a a')
mgu (TG.TVr x)   a'             = SJust (Ext (SPair x a')  Emp)
mgu (TG.Arr a b) (TG.Arr a' b') = case_Mgu b b' (mgu a a')
mgu (TG.Tpl a b) (TG.Tpl a' b') = case_Mgu b b' (mgu a a')
mgu (TG.Ary a)   (TG.Ary a')    = mgu a a'
mgu (TG.Vct a)   (TG.Vct a')    = mgu a a'
mgu (TG.May a)   (TG.May a')    = mgu a a'
mgu TG.Wrd       TG.Wrd         = SJust Emp
mgu TG.Bol       TG.Bol         = SJust Emp
mgu TG.Flt       TG.Flt         = SJust Emp
mgu TG.Cmx       TG.Cmx         = SJust Emp
mgu _            _              = case obvious :: Mgu a a' :~:
                                                  Nothing of
  Rfl                          -> SNothing

case_Mgu :: TG.Typ b -> TG.Typ b' -> SMaybe (Env (SPair NG.Nat TG.Typ)) s ->
            SMaybe (Env (SPair NG.Nat TG.Typ)) (Case_Mgu b b' s)
case_Mgu b b' (SJust s) = fmapAppend s (mgu (subs s b) b')
case_Mgu _ _  SNothing  = SNothing

fmapAppend :: Env f xs -> SMaybe (Env f) mys ->
              SMaybe (Env f) (FMapAppend xs mys)
fmapAppend Emp           x          = x
fmapAppend xs@(Ext _ _)  (SJust ys) = SJust (add xs ys)
fmapAppend (Ext _ _)     SNothing   = SNothing

appFul :: forall a a'.
          TG.Typ a -> Env TG.Typ a' -> SMaybe TG.Typ (AppFul a a')
appFul (TG.Arr a b) (Ext a' as) = case_AppFul (mgu a a') b as
appFul (TG.Arr _ _) Emp         = SNothing
appFul a            Emp         = case obvious :: AppFul a '[] :~:
                                                  Just a  of
  Rfl                          -> SJust a
appFul _            (Ext _ _)   = case obvious :: AppFul a a' :~:
                                                  Nothing  of
  Rfl                          -> SNothing

case_AppFul :: SMaybe (Env (SPair NG.Nat TG.Typ)) s -> TG.Typ b ->
               Env TG.Typ as -> SMaybe TG.Typ (Case_AppFul s b as)
case_AppFul (SJust s) b as = appFul (subs s b) as
case_AppFul SNothing  _ _  = SNothing

data SBool b where
  SFalse :: SBool False
  STrue :: SBool True

case_Match :: SMaybe TG.Typ c -> TG.Typ b -> SBool (Case_Match c b)
case_Match (SJust c) b = toBool (mgu c b)
case_Match SNothing  _ = SFalse

toBool :: SMaybe f a -> SBool (ToBool a)
toBool (SJust _) = STrue
toBool SNothing  = SFalse
