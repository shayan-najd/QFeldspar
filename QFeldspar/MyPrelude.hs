module QFeldspar.MyPrelude
       (Num(..),pi,fromIntegral,toRational
       ,floor,log,sqrt,
        Word32,Rational,
        Float,
        Bool(..),(&&),(||),not,
        Complex(..),realPart,imagPart,cis,magnitude,
        (.&.),(.|.),xor,shiftR,shiftL,complement,popCountDefault,testBit,
        IO,print,readFile,writeFile,putStrLn,getArgs,
        fst,snd,
        String,lines,unlines,
        read,
        (.),flip,curry,uncurry,id,const,
        (++),zip,head,dropWhile,iterate,length,zipWith,filter,(!!),delete,init,
        ord,
        Maybe(..),fromJust,maybe,
        Enum(..),
        Ord(..),
        Eq(..),
        Show(..),Fractional(..),Integral(..),
        otherwise, impossible , impossibleM,badUse,badTypVal,badTypValM,
        lookup,
        State,getState,put,modify,runState,execState,evalState,StateT
             ,lift,runStateT,evalStateT,
        genNewNam,deepseq,($),
        module QFeldspar.ErrorMonad,
        module QFeldspar.Existential,
        module QFeldspar.TraversalGenerator,
        module Data.Monoid,
        module Data.Foldable,
        module Data.Traversable,
        module Data.Functor,
        module Control.Applicative,
        module Control.Monad,
        module Data.Array,
        Bol,Ary,May,Cmx,Flt,Int,Arr,Tpl,Vec(..),
        cnd,while,tpl,arr,arrLen,arrIx,cmx,non,som,may)
       where
import Control.DeepSeq
import Prelude hiding (Int)
import QFeldspar.Existential
import Data.Maybe
import QFeldspar.ErrorMonad
import Data.Array
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Data.Functor
import Control.Monad hiding (forM,forM_,sequence,sequence_,msum,mapM,mapM_)
import Data.Monoid
import Control.Monad.State
import Data.List
import Data.Complex
import Data.Word
import Data.Bits
import Data.Char(ord)
import System.Environment
import Data.Unique
import System.IO.Unsafe
import QFeldspar.TraversalGenerator

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

type Int     = Word32
type Flt     = Float
type Bol     = Bool
type Ary a   = Array Word32 a
type May a   = Maybe a
type Cmx     = Complex Flt
type Arr a b = a -> b
type Tpl a b = (a , b)
data Vec a   = Vec Int (Int -> a)

{-# NOINLINE genNewNam #-}
genNewNam :: String -> String
genNewNam x = unsafePerformIO (fmap (x ++) (fmap (show . hashUnique) newUnique))

cnd :: Bool -> s -> s -> s
cnd c t f = if c then t else f

while :: (s -> Bool) -> (s -> s) -> s -> s
while fc fb = head . dropWhile fc . iterate fb

tpl :: a -> b -> (a , b)
tpl = ((,))

arr :: Int -> (Int -> a) -> Array Int a
arr l f = listArray (0 , l - 1) (fmap f [0 .. l - 1])

arrLen :: (Array Int a) -> Int
arrLen = (1 +) . uncurry (flip (-)) . bounds

arrIx :: (Array Int a) -> Int -> a
arrIx = (!)

cmx :: Float -> Float -> Complex Float
cmx = (:+)

non :: Maybe a
non = Nothing

som :: a -> Maybe a
som = Just

may :: Maybe a -> b -> (a -> b) -> b
may em en es = maybe en es em
