module QFeldspar.MyPrelude
  ( Word32,Rational,Integer,Float,Char,
   Bool(..),(&&),(||),not,
   Complex,realPart,imagPart,
   Maybe(..),fromJust,maybe,
   IO,print,readFile,writeFile,putStrLn,getArgs,
   fst,snd,
   String,lines,unlines,
   read,
   (.),flip,curry,uncurry,id,const,
   (++),zip,head,dropWhile,iterate,length,zipWith,filter,(!!),nub,init,
   delete,
   ord,
   Num(..),fromIntegral,toRational,
   Eq(..),Ord(..),Enum(..),Show(..),Fractional(..),Integral(..),
   otherwise,impossible,impossibleM,badUse,badTypVal,badTypValM,
   frmRgtZro,
   lookup,
   State,getState,put,modify,runState,execState,evalState,StateT(..),
   lift,evalStateT,
   module Data.Monoid,
   module Data.Foldable,
   module Data.Traversable,
   module Data.Functor,
   module Control.Applicative,
   module Control.Monad,
   Array,Ary,bounds,assocs,
   Vec(..),TVr(..),
   module QFeldspar.ErrorMonad,
   module QFeldspar.NameMonad,
   module QFeldspar.Existential,
   module QFeldspar.TraversalGenerator,
   trace)
  where

import Debug.Trace
import Prelude hiding (Int,mapM,sequence)
import QFeldspar.Nat.GADT
import QFeldspar.Existential
import Data.Maybe
import QFeldspar.ErrorMonad
import QFeldspar.NameMonad
import Data.Array
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Data.Functor
import Control.Monad hiding
   (forM,forM_,sequence,sequence_,msum,mapM,mapM_)
import Data.Monoid
import Control.Monad.State hiding (mapM,sequence)
import Data.List
import Data.Complex
import Data.Word
import Data.Char(ord)
import System.Environment
import QFeldspar.TraversalGenerator

frmRgtZro :: NamM ErrM a -> a
frmRgtZro = frmRgt . runNamM

impossible :: a
impossible = error "Impossible!"

impossibleM :: Monad m => m a
impossibleM = fail "Impossible!"

badUse :: String -> a
badUse = error . ("Bad use of " ++)

badTypVal :: a
badTypVal = error "Value of wrong type!"

badTypValM :: Monad m => m a
badTypValM = fail "Value of wrong type!"

getState :: MonadState s m => m s
getState = get

type Ary a = Array Word32 a
data Vec a = Vec Word32 (Word32 -> a)
data TVr x = TVar (Nat x)
