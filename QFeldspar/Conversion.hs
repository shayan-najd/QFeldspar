module QFeldspar.Conversion(Cnv(..),cnvWth) where

import QFeldspar.MyPrelude

class Cnv a b where
  cnv :: a -> NamM ErrM b

instance Cnv (a, r) a where
  cnv (x , _) = pure x

cnvWth :: Cnv (a , g) b => g -> a -> NamM ErrM b
cnvWth g a = cnv (a , g)
