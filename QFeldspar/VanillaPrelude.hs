module QFeldspar.VanillaPrelude where

import QFeldspar.MyPrelude as MP

cnd :: Bool -> s -> s -> s
cnd c t f = if c then t else f

whl :: (s -> Bool) -> (s -> s) -> s -> s
whl fc fb = head . dropWhile fc . iterate fb

tpl :: a -> b -> (a , b)
tpl = ((,))

fst :: (a , b) -> a
fst = MP.fst

snd :: (a , b) -> b
snd = MP.snd

ary :: Int -> (Int -> a) -> Array Int a
ary l f = listArray (0 , l - 1) (fmap f [0 .. l - 1])

len :: (Array Int a) -> Int
len = (1 +) . uncurry (flip (-)) . bounds

ind :: (Array Int a) -> Int -> a
ind = (!)

cmx :: Float -> Float -> Complex Float
cmx = (:+)

non :: Maybe a
non = MP.Nothing

som :: a -> Maybe a
som = MP.Just

may :: Maybe a -> b -> (a -> b) -> b
may em en es = MP.maybe en es em