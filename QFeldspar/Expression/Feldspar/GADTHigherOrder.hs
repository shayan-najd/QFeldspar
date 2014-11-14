module QFeldspar.Expression.Feldspar.GADTHigherOrder
       (Exp(..)) where

import QFeldspar.MyPrelude
import QFeldspar.Variable.Typed
import qualified QFeldspar.Type.Feldspar.GADT as TFG
import QFeldspar.Singleton

data Exp :: [*] -> * -> * where
  ConI :: Int      -> Exp r Int
  ConB :: Bool     -> Exp r Bol
  ConF :: Float    -> Exp r Flt
  Var  :: Var r t  -> Exp r t
  Abs  :: (Exp r ta -> Exp r tb) -> Exp r (Arr ta tb)
  App  :: HasSin TFG.Typ ta =>
          Exp r (Arr ta tb) -> Exp r ta -> Exp r tb
  Cnd  :: Exp r Bol -> Exp r t -> Exp r t -> Exp r t
  Whl  :: Exp r (Arr t Bol) -> Exp r (Arr t t) -> Exp r t -> Exp r t
  Tpl  :: Exp r tf -> Exp r ts -> Exp r (Tpl tf ts)
  Fst  :: HasSin TFG.Typ ts => Exp r (Tpl tf ts) -> Exp r tf
  Snd  :: HasSin TFG.Typ tf => Exp r (Tpl tf ts) -> Exp r ts
  Ary  :: Exp r Int -> Exp r (Arr Int t) -> Exp r (Ary t)
  Len  :: HasSin TFG.Typ ta => Exp r (Ary ta) -> Exp r Int
  Ind  :: Exp r (Ary ta) -> Exp r Int -> Exp r ta
  AryV :: Exp r Int -> Exp r (Arr Int t) -> Exp r (Vec t)
  LenV :: HasSin TFG.Typ ta => Exp r (Vec ta) -> Exp r Int
  IndV :: Exp r (Vec ta) -> Exp r Int -> Exp r ta
  Let  :: HasSin TFG.Typ tl =>
          Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb
  Cmx  :: Exp r Flt -> Exp r Flt -> Exp r Cmx
  Non  :: Exp r (May tl)
  Som  :: Exp r tl -> Exp r (May tl)
  May  :: HasSin TFG.Typ a =>
          Exp r (May a) -> Exp r b -> Exp r (Arr a b) -> Exp r b
  Mul  :: Exp r a  -> Exp r a -> Exp r a
  Tmp  :: String -> Exp r a

deriving instance Show (Exp r t)

instance Show (Exp r ta -> Exp r tb) where
  show f =
    let v = genNewNam "x"
        {-# NOINLINE v #-}
    in deepseq v $ ("(\\ "++ v ++ " -> (" ++
        show (f (Tmp v))
        ++ "))")
