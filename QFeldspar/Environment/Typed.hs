module QFeldspar.Environment.Typed
    (Env(Emp,Ext),fmap,foldMap,foldl,traverse,len,lenNat,get,add,Get) where

import QFeldspar.MyPrelude hiding (mapM,fmap,traverse,foldMap,foldl)

import QFeldspar.Variable.Typed

import QFeldspar.Singleton
import qualified QFeldspar.Nat.ADT  as NA
import qualified QFeldspar.Nat.GADT as NG

data Env :: (k -> *) -> [k] -> * where
  Emp :: Env tf '[]
  Ext :: tf t -> Env tf e -> Env tf (t ': e)

fmap :: (forall t. tfa t -> tfb t) -> Env tfa r -> Env tfb r
fmap _ Emp        = Emp
fmap f (Ext x xs) = Ext (f x) (fmap f xs)

foldMap :: Monoid m  =>
           (forall t. tfa t -> m) -> Env tfa r -> m
foldMap  _ Emp        = mempty
foldMap  f (Ext x xs) = mappend (f x) (foldMap f xs)

foldl :: (forall t. b -> tfa t -> b) -> b -> Env tfa r -> b
foldl _ z Emp        = z
foldl f z (Ext x xs) = foldl f (f z x) xs

traverse :: Applicative m =>
            (forall t. tfa t -> m (tfb t)) -> Env tfa r -> m (Env tfb r)
traverse _ Emp        = pure Emp
traverse f (Ext x xs) = Ext <$> f x <*> traverse f xs

len :: Env ef r -> NG.Nat (Len r)
len Emp        = NG.Zro
len (Ext _ xs) = NG.Suc (len xs)

lenNat :: Env ef r -> NA.Nat
lenNat Emp        = NA.Zro
lenNat (Ext _ xs) = NA.Suc (lenNat xs)

get :: Var r t -> Env tf r -> tf t
get Zro     (Ext x  _ ) = x
get (Suc n) (Ext _  xs) = get n xs
get _       Emp         = impossible

add :: Env t ra -> Env t rb -> Env t (Add ra rb)
add Emp        ys = ys
add (Ext x xs) ys = Ext x (add xs ys)

type family Get (ls :: [k]) (n :: NA.Nat) :: k where
    Get (x ': xs) 'NA.Zro     = x
    Get (x ': xs) ('NA.Suc n) = Get xs n

instance HasSin (Env tf) '[] where
  sin = Emp

instance (HasSin tf t , HasSin (Env tf) ts) => HasSin (Env tf) (t ': ts) where
  sin = Ext sin sin

instance EqlSin tf => EqlSin (Env tf) where
  eqlSin Emp       Emp         = return Rfl
  eqlSin (Ext t e) (Ext t' e') = do Rfl <- eqlSin e e'
                                    Rfl <- eqlSin t t'
                                    return Rfl
  eqlSin  _           _        = fail "Scope Error!"

instance GetPrfHasSin tf => GetPrfHasSin (Env tf) where
  getPrfHasSin Emp        = PrfHasSin
  getPrfHasSin (Ext t ts) = case getPrfHasSin ts of
    PrfHasSin   -> case getPrfHasSin t of
      PrfHasSin -> PrfHasSin
