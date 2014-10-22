module QFeldspar.Normalization
       (NrmOne(..),(<$>),(<*>),pure,(<$@>),(<*@>),Chg(..),chg,nrm)
       where

import QFeldspar.MyPrelude
import QFeldspar.ChangeMonad

class NrmOne a where
  nrmOne :: a -> Chg a

nrm :: NrmOne a => a -> a
nrm = tilNotChg nrmOne

infixl 4 <$@>
(<$@>) :: NrmOne a => (a -> b) -> a -> Chg b
el <$@> er = el <$> nrmOne er

infixl 4 <*@>
(<*@>) :: NrmOne a => Chg (a -> b) -> a -> Chg b
el <*@> er = el <*> nrmOne er