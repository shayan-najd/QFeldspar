module QFeldspar.Nat.Conversion () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Nat.ADT  as NA
import qualified QFeldspar.Nat.GADT as NG
import qualified Prelude as P

import QFeldspar.Conversion

instance Cnv (NA.Nat , r) (ExsSin NG.Nat) where
  cnv (ee , r) = case ee of
    NA.Zro     -> return (ExsSin NG.Zro)
    (NA.Suc n) -> do ExsSin n' <- cnvWth r n
                     return (ExsSin (NG.Suc n'))

instance Cnv NA.Nat P.Int where
  cnv NA.Zro     = return 0
  cnv (NA.Suc v) = (1 +) <$> cnv v

instance Cnv P.Int NA.Nat where
  cnv 0         = return NA.Zro
  cnv x
    | x > 0     = NA.Suc <$> cnv (pred x)
    | otherwise = fail "Conversion Error!"

instance Enum NA.Nat where
  fromEnum = frmRgtZro . cnv
  toEnum   = frmRgtZro . cnv
