module QFeldspar.Expression.MiniFeldspar
      (Exp(..)) where

import QFeldspar.MyPrelude
import qualified QFeldspar.Type.GADT     as TFG

import QFeldspar.Variable.Typed

import QFeldspar.Environment.Typed as ET
import QFeldspar.Singleton

data Exp :: [*] -> * -> * where
  ConI  :: Int      -> Exp r Int
  ConB  :: Bool     -> Exp r Bol
  ConF  :: Float    -> Exp r Flt
  AppV  :: HasSin TFG.Typ t =>
           Var r t  -> Env (Exp r) (TFG.Arg t) -> Exp r (TFG.Out t)
  Cnd   :: Exp r Bol -> Exp r t -> Exp r t -> Exp r t
  Whl   :: (Exp r t -> Exp r Bol) -> (Exp r t -> Exp r t) ->
           Exp r t  -> Exp r t
  Tpl   :: Exp r tf -> Exp r ts -> Exp r (Tpl tf ts)
  Fst   :: HasSin TFG.Typ ts =>
           Exp r (Tpl tf ts)-> Exp r tf
  Snd   :: HasSin TFG.Typ tf =>
           Exp r (Tpl tf ts)-> Exp r ts
  Ary   :: Exp r Int -> (Exp r Int -> Exp r t) -> Exp r (Ary t)
  Len   :: HasSin TFG.Typ ta =>
           Exp r (Ary ta) -> Exp r Int
  Ind   :: Exp r (Ary ta) -> Exp r Int -> Exp r ta
  Let   :: HasSin TFG.Typ tl =>
           Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb
  Cmx   :: Exp r Flt -> Exp r Flt -> Exp r Cmx
  Tmp   :: String -> Exp r t  -- dummy constructor
  Tag   :: String -> Exp r t -> Exp r t
  Mul   :: Exp r t -> Exp r t -> Exp r t
  Add   :: Exp r t -> Exp r t -> Exp r t
  Sub   :: Exp r t -> Exp r t -> Exp r t
  Eql   :: HasSin TFG.Typ t => Exp r t -> Exp r t -> Exp r Bool
  Ltd   :: HasSin TFG.Typ t => Exp r t -> Exp r t -> Exp r Bool
  Mem   :: Exp r t -> Exp r t
