module QFeldspar.Expression.Utils.Show.GADTFirstOrder
       () where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTFirstOrder as FGFO
import QFeldspar.Environment.Typed

deriving instance Show (Exp s g a)

instance Show (Env (Exp s g) g') where
  show Emp        = "Emp"
  show (Ext x xs) = "Ext (" ++ show x ++") ("++ show xs ++")"
