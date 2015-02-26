module QFeldspar.Expression.GADTValue
    (Exp(..)
    ,conI,conB,conF,var,abs,app,cnd,whl,tpl,fst,snd,ary,len,ind,leT
    ,cmx,tag,mul,add,int,mem
    ,getTrm) where

import QFeldspar.MyPrelude hiding (abs,fst,snd,may,som,non,cmx,tpl,cnd,mem)
import qualified QFeldspar.MyPrelude as MP
import QFeldspar.Type.GADT ()

data Exp :: * -> * where
  Exp :: t -> Exp t

deriving instance Functor Exp

getTrm :: Exp t -> t
getTrm (Exp x) = x

prm0 :: a -> Exp a
prm0 = Exp

prm1 :: (a -> b) -> Exp a -> Exp b
prm1 f = fmap f

prm2 :: (a -> b -> c) ->
        Exp a -> Exp b -> Exp c
prm2 f e1 e2 = let e1' = getTrm e1
                   e2' = getTrm e2
               in Exp (f e1' e2')

prm3 :: (a -> b -> c -> d) ->
        Exp a -> Exp b -> Exp c -> Exp d
prm3 f e1 e2 e3 = let e1' = getTrm e1
                      e2' = getTrm e2
                      e3' = getTrm e3
                  in  Exp (f e1' e2' e3')

var :: t -> t
var = id

conI :: Int -> Exp Int
conI = prm0

conB :: Bool -> Exp Bol
conB = prm0

conF :: Float -> Exp Flt
conF = prm0

abs :: Exp (Arr ta tb) -> Exp (Arr ta tb)
abs = id

app :: Exp (Arr ta tb) -> Exp ta -> Exp tb
app = prm2 (\ f x -> f x)

cnd :: Exp Bol -> Exp a -> Exp a -> Exp a
cnd = prm3 MP.cnd

whl :: Exp (Arr s  Bol) -> Exp (Arr s s) -> Exp s -> Exp s
whl = prm3 MP.while

tpl :: Exp tf -> Exp ts -> Exp (Tpl tf ts)
tpl = prm2 MP.tpl

fst :: Exp (Tpl a b) -> Exp a
fst = prm1 MP.fst

snd :: Exp (Tpl a b) -> Exp b
snd = prm1 MP.snd

ary :: Exp Int -> Exp (Arr Int a) -> Exp (Ary a)
ary = prm2 MP.mkArr

len :: Exp (Ary a) -> Exp Int
len = prm1 MP.lnArr

ind :: Exp (Ary a) -> Exp Int -> Exp a
ind = prm2 MP.ixArr

leT :: Exp tl -> Exp (Arr tl tb) -> Exp tb
leT = prm2 (\ x f -> f x)

cmx :: Exp Flt -> Exp Flt -> Exp Cmx
cmx = prm2 MP.cmx

mul :: Num a => Exp a -> Exp a -> Exp a
mul = prm2 (*)

add :: Num a => Exp a -> Exp a -> Exp a
add = prm2 (+)

tag :: String -> Exp a -> Exp a
tag = const id

int :: Num a => Int -> Exp a
int = Exp . fromIntegral

mem :: Exp a -> Exp a
mem = id
