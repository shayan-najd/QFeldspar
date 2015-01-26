module QFeldspar.Expression.Utils.TemplateHaskell ((===),unQ,stripNameSpace,trmEql)
    where

import Prelude
import QFeldspar.ErrorMonad
import Language.Haskell.TH.Syntax hiding (unQ)

(===) :: Name -> Name -> Bool
n1 === n2 = stripNameSpace n1 == stripNameSpace n2

unQ :: Q a -> a
unQ = frmRgt. runQ

stripNameSpace :: Name -> Name
stripNameSpace (Name x _) = Name x NameS

trmEql :: Q (TExp a) -> Q (TExp b) -> Bool
trmEql m n = let m' = frmRgt (runQ (unTypeQ m))
                 n' = frmRgt (runQ (unTypeQ n))
             in m' == n'

instance Show (Q (TExp a)) where
    show q = let q' = frmRgt (runQ (unTypeQ q))
             in show q'

instance Quasi ErrM where
  qNewName          = return . mkName
  qReport b         = fail . if b
                             then ("Error: " ++)
                             else ("Warning: " ++)
  qRecover          = fail "Not Allowed!"
  qReify            = fail "Not Allowed!"
  qLookupName       = fail "Not Allowed!"
  qReifyInstances   = fail "Not Allowed!"
  qReifyRoles       = fail "Not Allowed!"
  qReifyAnnotations = fail "Not Allowed!"
  qReifyModule      = fail "Not Allowed!"
  qAddDependentFile = fail "Not Allowed!"
  qAddModFinalizer  = fail "Not Allowed!"
  qAddTopDecls      = fail "Not Allowed!"
  qLocation         = fail "Not Allowed!"
  qRunIO            = fail "Not Allowed!"
  qPutQ             = fail "Not Allowed!"
  qGetQ             = fail "Not Allowed!"
