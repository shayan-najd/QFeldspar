module QFeldspar.Expression.GADTFirstOrder
      (Exp(..)) where

import QFeldspar.MyPrelude
import QFeldspar.Environment.Typed
import QFeldspar.Variable.Typed
import QFeldspar.Type.GADT

data Exp :: [*] -> [*] -> * -> * where
  ConI :: Word32   -> Exp s g Word32
  ConB :: Bool     -> Exp s g Bool
  ConF :: Float    -> Exp s g Float
  Var  :: Var g a  -> Exp s g a
  Prm  :: Types as =>
          Var s (as :-> b) -> Env (Exp s g) as -> Exp s g b
  Abs  :: Exp s (a ': g) b -> Exp s g (a -> b)
  App  :: Type a =>
          Exp s g (a -> b) -> Exp s g a -> Exp s g b
  Cnd  :: Exp s g Bool -> Exp s g a -> Exp s g a -> Exp s g a
  Whl  :: Exp s g (a -> Bool) ->
          Exp s g (a -> a) -> Exp s g a -> Exp s g a
  Tpl  :: Exp s g a -> Exp s g b -> Exp s g (a , b)
  Fst  :: Type b => Exp s g (a , b) -> Exp s g a
  Snd  :: Type a => Exp s g (a , b) -> Exp s g b
  Ary  :: Exp s g Word32 -> Exp s g (Word32 -> a) -> Exp s g (Ary a)
  Len  :: Type a => Exp s g (Ary a) -> Exp s g Word32
  Ind  :: Exp s g (Ary a) -> Exp s g Word32 -> Exp s g a
  AryV :: Exp s g Word32 -> Exp s g (Word32 -> a) -> Exp s g (Vec a)
  LenV :: Type a => Exp s g (Vec a) -> Exp s g Word32
  IndV :: Exp s g (Vec a) -> Exp s g Word32 -> Exp s g a
  LeT  :: Type a => Exp s g a -> Exp s (a ': g) b -> Exp s g b
  Cmx  :: Exp s g Float -> Exp s g Float -> Exp s g (Complex Float)
  Non  :: Exp s g (Maybe a)
  Som  :: Exp s g a -> Exp s g (Maybe a)
  May  :: Type a => Exp s g (Maybe a) ->
          Exp s g b -> Exp s g (a -> b) -> Exp s g b
  Mul  :: Exp s g a  -> Exp s g a -> Exp s g a
  Add  :: Exp s g a  -> Exp s g a -> Exp s g a
  Sub  :: Exp s g a  -> Exp s g a -> Exp s g a
  Eql  :: Type a => Exp s g a  -> Exp s g a -> Exp s g Bool
  Ltd  :: Type a => Exp s g a  -> Exp s g a -> Exp s g Bool
  Int  :: Word32 -> Exp s g a
  Tag  :: String -> Exp s g a -> Exp s g a
  Mem  :: Exp s g a -> Exp s g a
  Fix  :: Exp s g (a -> a) -> Exp s g a
