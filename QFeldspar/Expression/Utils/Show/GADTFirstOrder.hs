module QFeldspar.Expression.Utils.Show.GADTFirstOrder
       () where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTFirstOrder as FGFO

deriving instance Show (Exp g a)
