module QFeldspar.Literal.GADT where

import QFeldspar.MyPrelude

data Lit a where
  CharL     :: Char     -> Lit Char
  StringL   :: String   -> Lit String
  IntegerL  :: Integer  -> Lit Integer
  RationalL :: Rational -> Lit Rational

unLit :: Lit a -> a
unLit l = case l of
  CharL     c -> c
  StringL   s -> s
  IntegerL  i -> i
  RationalL r -> r

deriving instance Show (Lit a)
deriving instance Eq   (Lit a)
