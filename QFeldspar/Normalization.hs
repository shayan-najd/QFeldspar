module QFeldspar.Normalization
       (NrmOne(..),(<$>),(<*>),pure,(<$@>),(<*@>),Chg(..),chg,nrm,nrmIf)
       where

import QFeldspar.MyPrelude

import qualified QFeldspar.Variable.Plain  as VP
import qualified QFeldspar.Variable.Scoped as VS
import qualified QFeldspar.Variable.Typed  as VT
import QFeldspar.ChangeMonad

class NrmOne a where
  nrmOne :: a -> Chg a

instance NrmOne Int          where
  nrmOne = pure

instance NrmOne Bool         where
  nrmOne = pure

instance NrmOne Float        where
  nrmOne = pure

instance NrmOne VP.Var       where
  nrmOne = pure

instance NrmOne (VS.Var n)   where
  nrmOne = pure

instance NrmOne (VT.Var r t) where
  nrmOne = pure

nrm :: NrmOne a => a -> a
nrm = tilNotChg nrmOne

nrmIf :: NrmOne a => Bool -> a -> a
nrmIf True = nrm
nrmIf False = id

infixl 4 <$@>
(<$@>) :: NrmOne a => (a -> b) -> a -> Chg b
el <$@> er = el <$> nrmOne er

infixl 4 <*@>
(<*@>) :: NrmOne a => Chg (a -> b) -> a -> Chg b
el <*@> er = el <*> nrmOne er