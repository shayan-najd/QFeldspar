module QFeldspar.Expression.Utils.Show.Common where

import QFeldspar.MyPrelude
-- import qualified QFeldspar.Type.GADT as TG
import QFeldspar.Environment.Typed hiding (fmap)

data TT a b = TT {unTT :: a}

instance Show (Env (TT String) r') where
    show Emp        = "Emp"
    show (Ext s ss) = "\n Ext (" ++ unTT s ++ ") (" ++ show ss ++ ")"
