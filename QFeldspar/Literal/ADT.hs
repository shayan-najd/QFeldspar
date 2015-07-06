module QFeldspar.Literal.ADT where

import QFeldspar.MyPrelude

data Lit where
  CharL     :: Char     -> Lit
  StringL   :: String   -> Lit
  IntegerL  :: Integer  -> Lit
  RationalL :: Rational -> Lit

deriving instance Show Lit
deriving instance Eq   Lit
