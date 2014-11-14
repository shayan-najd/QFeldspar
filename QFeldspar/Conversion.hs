module QFeldspar.Conversion(Cnv(..),(<$@>),(<*@>),cnvImp) where

import QFeldspar.MyPrelude

class Cnv a b where
  cnv :: a -> ErrM b

instance Cnv (a, r) a where
  cnv (x , _) = pure x
 
cnvImp :: (Cnv (a , r) b , ?r :: r) => a -> ErrM b
cnvImp x = cnv (x  , ?r)

infixl 4 <$@>
(<$@>) :: (?r :: r , Cnv (a , r) a') => (a' -> b) -> a -> ErrM b
el <$@> er = el <$> cnv (er , ?r)

infixl 4 <*@>
(<*@>) :: (?r :: r , Cnv (a , r) a') => ErrM (a' -> b) -> a -> ErrM b
el <*@> er = el <*> cnv (er , ?r)
