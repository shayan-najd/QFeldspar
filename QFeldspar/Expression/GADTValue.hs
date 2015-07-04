module QFeldspar.Expression.GADTValue
    (Exp(..)
    ,conI,conB,conF,prm,var,abs,app,cnd,whl,tpl,fst,snd,ary,len,ind,leT
    ,cmx,tag,mul,add,sub,eql,ltd,int,mem,fix,aryV,lenV,indV,non,som,may
    ,getTrm) where

import QFeldspar.MyPrelude hiding (abs,fst,snd)
import qualified QFeldspar.Type.GADT as TG
import qualified QFeldspar.Environment.Typed as ET
import QFeldspar.Singleton
import QFeldspar.Magic
import Unsafe.Coerce
import qualified QFeldspar.Prelude.Haskell as PH

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

conI :: Word32 -> Exp Word32
conI = prm0

conB :: Bool -> Exp Bool
conB = prm0

conF :: Float -> Exp Float
conF = prm0

abs :: Exp (ta -> tb) -> Exp (ta -> tb)
abs = id

app :: Exp (ta -> tb) -> Exp ta -> Exp tb
app = prm2 (\ f x -> f x)

-- The functionunsafeCoerce is "safe" to be used here,
-- since type-safety is guaranteed by Trm datatype.
prm :: forall a as b. Match a as b => Exp a -> ET.Env Exp as -> Exp b
prm f xss = unsafeCoerce(ET.foldl (\ (Exp f') (Exp x) ->
                                    Exp ((unsafeCoerce f') x)) f xss)

cnd :: Exp Bool -> Exp a -> Exp a -> Exp a
cnd = prm3 PH.cnd

whl :: Exp (s -> Bool) -> Exp (s -> s) -> Exp s -> Exp s
whl = prm3 PH.while

tpl :: Exp tf -> Exp ts -> Exp (tf , ts)
tpl = prm2 PH.tpl

fst :: Exp (a , b) -> Exp a
fst = prm1 PH.fst

snd :: Exp (a , b) -> Exp b
snd = prm1 PH.snd

ary :: Exp Word32 -> Exp (Word32 -> a) -> Exp (Ary a)
ary = prm2 PH.mkArr

len :: Exp (Ary a) -> Exp Word32
len = prm1 PH.lnArr

ind :: Exp (Ary a) -> Exp Word32 -> Exp a
ind = prm2 PH.ixArr

leT :: Exp tl -> Exp (tl -> tb) -> Exp tb
leT = prm2 (\ x f -> f x)

cmx :: Exp Float -> Exp Float -> Exp (Complex Float)
cmx = prm2 (PH.cmx)

mul :: forall a. TG.Type a => Exp a -> Exp a -> Exp a
mul = case sin :: TG.Typ a of
  TG.Wrd -> prm2 (*)
  TG.Flt -> prm2 (*)
  TG.Cmx -> prm2 (*)
  _      -> badTypVal

add :: forall a. TG.Type a => Exp a -> Exp a -> Exp a
add = case sin :: TG.Typ a of
  TG.Wrd -> prm2 (+)
  TG.Flt -> prm2 (+)
  TG.Cmx -> prm2 (+)
  _      -> badTypVal

sub :: forall a. TG.Type a => Exp a -> Exp a -> Exp a
sub = case sin :: TG.Typ a of
  TG.Wrd -> prm2 (-)
  TG.Flt -> prm2 (-)
  TG.Cmx -> prm2 (-)
  _      -> badTypVal

eql :: forall a. TG.Type a => Exp a -> Exp a -> Exp Bool
eql = case sin :: TG.Typ a of
  TG.Wrd -> prm2 (==)
  TG.Flt -> prm2 (==)
  TG.Bol -> prm2 (==)
  _      -> badTypVal

ltd :: forall a. TG.Type a => Exp a -> Exp a -> Exp Bool
ltd = case sin :: TG.Typ a of
  TG.Wrd -> prm2 (<)
  TG.Flt -> prm2 (<)
  TG.Bol -> prm2 (<)
  _      -> badTypVal

tag :: String -> Exp a -> Exp a
tag = const id

int :: forall a. TG.Type a => Word32 -> Exp a
int = case sin :: TG.Typ a of
  TG.Wrd -> Exp . fromIntegral
  TG.Flt -> Exp . fromIntegral
  _      -> badTypVal

mem :: Exp a -> Exp a
mem = id

fix :: Exp (a -> a) -> Exp a
fix = prm1 PH.fix

aryV :: Exp Word32 -> Exp (Word32 -> a) -> Exp (Vec a)
aryV = prm2 PH.Vec

lenV :: Exp (Vec a) -> Exp Word32
lenV = prm1 PH.lnVec

indV :: Exp (Vec a) -> Exp Word32 -> Exp a
indV = prm2 PH.ixVec

non  :: Exp (Maybe a)
non  = prm0 Nothing

som  :: Exp a -> Exp (Maybe a)
som  = prm1 Just

may  :: Exp (Maybe a) ->
        Exp b -> Exp (a -> b) -> Exp b
may  = prm3 PH.may
