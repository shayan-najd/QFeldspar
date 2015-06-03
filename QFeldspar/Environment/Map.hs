module QFeldspar.Environment.Map (Env,pattern Emp,pattern Ext,len,get) where

import QFeldspar.MyPrelude

import qualified QFeldspar.Nat.ADT  as NA

type Env a b = [(a , b)]

pattern Emp      = []
pattern Ext x xs = x : xs

len :: Env a b -> NA.Nat
len Emp        = NA.Zro
len (Ext _ xs) = NA.Suc (len xs)
len _          = impossible

get :: (Monad m , Eq a , Show a) => a -> Env a b -> m b
get x xs = maybe (fail ("Scope Error: cannot find '"++ show x ++ "'")) return (lookup x xs)
