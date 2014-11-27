module QFeldspar.Conversion(Cnv(..),(<$@>),(<*@>),cnvImp) where

import QFeldspar.MyPrelude

class Cnv a b where
  cnv :: a -> NamM ErrM b

instance Cnv (a, r) a where
  cnv (x , _) = pure x

cnvImp :: (Cnv (a , r) b , ?r :: r) => a -> NamM ErrM b
cnvImp x = cnv (x  , ?r)

infixl 4 <$@>
(<$@>) :: (?r :: r , Cnv (a , r) a') => (a' -> b) -> a -> NamM ErrM b
el <$@> er = el <$> cnv (er , ?r)

infixl 4 <*@>
(<*@>) :: (?r :: r , Cnv (a , r) a') => NamM ErrM (a' -> b) -> a -> NamM ErrM b
el <*@> er = el <*> cnv (er , ?r)
