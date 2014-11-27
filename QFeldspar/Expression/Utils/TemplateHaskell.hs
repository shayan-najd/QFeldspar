module QFeldspar.Expression.Utils.TemplateHaskell
       (trmEql) where

import QFeldspar.MyPrelude
import Language.Haskell.TH.Syntax

trmEql :: Q (TExp a) -> Q (TExp b) -> Bool
trmEql m n = let m' = frmRgt (runQ (unTypeQ m))
                 n' = frmRgt (runQ (unTypeQ n))
             in m' == n'

instance Show (Q (TExp a)) where
    show q = let q' = frmRgt (runQ (unTypeQ q))
             in show q'
