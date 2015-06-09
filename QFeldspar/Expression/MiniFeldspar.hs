module QFeldspar.Expression.MiniFeldspar
       (Exp(..)) where

import QFeldspar.MyPrelude
import QFeldspar.Environment.Typed
import QFeldspar.Variable.Typed
import QFeldspar.Type.GADT
import QFeldspar.Magic

data Exp :: [*] -> * -> * where
  ConI  :: Word32 -> Exp s Word32
  ConB  :: Bool   -> Exp s Bool
  ConF  :: Float  -> Exp s Float
  Prm   :: (Match a as b , Types as) =>
           Var s a -> Env (Exp s) as -> Exp s b
  Cnd   :: Exp s Bool -> Exp s a -> Exp s a -> Exp s a
  Whl   :: (Exp s a -> Exp s Bool) -> (Exp s a -> Exp s a) ->
           Exp s a  -> Exp s a
  Tpl   :: Exp s a -> Exp s b -> Exp s (a , b)
  Fst   :: Type b =>
           Exp s (a , b)-> Exp s a
  Snd   :: Type a =>
           Exp s (a , b)-> Exp s b
  Ary   :: Exp s Word32 -> (Exp s Word32 -> Exp s a) -> Exp s (Ary a)
  Len   :: Type a =>
           Exp s (Ary a) -> Exp s Word32
  Ind   :: Exp s (Ary a) -> Exp s Word32 -> Exp s a
  LeT   :: Type a =>
           Exp s a -> (Exp s a -> Exp s b) -> Exp s b
  Cmx   :: Exp s Float -> Exp s Float -> Exp s (Complex Float)
  Tmp   :: String -> Exp s a  -- dummy constructor
  Tag   :: String -> Exp s a -> Exp s a
  Mul   :: Exp s a -> Exp s a -> Exp s a
  Add   :: Exp s a -> Exp s a -> Exp s a
  Sub   :: Exp s a -> Exp s a -> Exp s a
  Eql   :: Type a => Exp s a -> Exp s a -> Exp s Bool
  Ltd   :: Type a => Exp s a -> Exp s a -> Exp s Bool
  Mem   :: Exp s a -> Exp s a
