module QFeldspar.Expression.GADTHigherOrder
       (Exp(..)) where

import QFeldspar.MyPrelude
import QFeldspar.Variable.Typed
import qualified QFeldspar.Type.GADT as TFG
import QFeldspar.Singleton

data Exp :: [*] -> * -> * where
  ConI :: Word32   -> Exp r Word32
  ConB :: Bool     -> Exp r Bool
  ConF :: Float    -> Exp r Float
  Var  :: Var r t  -> Exp r t
  Abs  :: (Exp r ta -> Exp r tb) -> Exp r (ta -> tb)
  App  :: HasSin TFG.Typ ta =>
          Exp r (ta -> tb) -> Exp r ta -> Exp r tb
  Cnd  :: Exp r Bool -> Exp r t -> Exp r t -> Exp r t
  Whl  :: Exp r (t -> Bool) -> Exp r (t -> t) -> Exp r t -> Exp r t
  Tpl  :: Exp r tf -> Exp r ts -> Exp r (tf , ts)
  Fst  :: HasSin TFG.Typ ts => Exp r (tf , ts) -> Exp r tf
  Snd  :: HasSin TFG.Typ tf => Exp r (tf , ts) -> Exp r ts
  Ary  :: Exp r Word32 -> Exp r (Word32 -> t) -> Exp r (Ary t)
  Len  :: HasSin TFG.Typ ta => Exp r (Ary ta) -> Exp r Word32
  Ind  :: Exp r (Ary ta) -> Exp r Word32 -> Exp r ta
  AryV :: Exp r Word32 -> Exp r (Word32 -> t) -> Exp r (Vec t)
  LenV :: HasSin TFG.Typ ta => Exp r (Vec ta) -> Exp r Word32
  IndV :: Exp r (Vec ta) -> Exp r Word32 -> Exp r ta
  Let  :: HasSin TFG.Typ tl =>
          Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb
  Cmx  :: Exp r Float -> Exp r Float -> Exp r (Complex Float)
  Non  :: Exp r (Maybe tl)
  Som  :: Exp r tl -> Exp r (Maybe tl)
  May  :: HasSin TFG.Typ a =>
          Exp r (Maybe a) -> Exp r b -> Exp r (a -> b) -> Exp r b
  Mul  :: Exp r a  -> Exp r a -> Exp r a
  Add  :: Exp r a  -> Exp r a -> Exp r a
  Sub  :: Exp r a  -> Exp r a -> Exp r a
  Eql  :: HasSin TFG.Typ a => Exp r a  -> Exp r a -> Exp r Bool
  Ltd  :: HasSin TFG.Typ a => Exp r a  -> Exp r a -> Exp r Bool
  Tmp  :: String -> Exp r a
  Int  :: Word32 -> Exp r a
  Tag  :: String -> Exp r t -> Exp r t
  Mem  :: Exp r a -> Exp r a
  Fix  :: Exp r (a -> a) -> Exp r a
