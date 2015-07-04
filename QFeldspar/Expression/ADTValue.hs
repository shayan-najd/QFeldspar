module QFeldspar.Expression.ADTValue
    (Exp
    ,conI,conB,conF,prm,var,abs,app,cnd,whl,tpl,fst,snd,ary,len,ind,leT
    ,cmx,typ,mul,add,sub,eql,ltd,int,mem,fix,aryV,lenV,indV,non,som,may
    ,Rep(..)) where


import QFeldspar.MyPrelude hiding (abs,fst,snd,Vec)
import qualified QFeldspar.Prelude.Haskell as PH
import qualified QFeldspar.Type.ADT as TA

data Exp = ConI Word32
         | ConB Bool
         | ConF Float
         | Abs (Exp -> Exp)
         | Tpl (Exp , Exp)
         | Ary (Ary Exp)
         | Cmx (Complex Float)
         | Vec (PH.Vec Exp)
         | May (Maybe Exp)

class Rep a where
  toExp  :: a -> Exp
  frmExp :: Exp -> ErrM a

instance Rep Exp where
  toExp  = id
  frmExp = pure

instance Rep Word32 where
  toExp            = ConI
  frmExp (ConI i) = return i
  frmExp _        = badTypValM

instance Rep Bool where
  toExp           = ConB
  frmExp (ConB b) = return b
  frmExp _        = badTypValM

instance Rep Float where
  toExp           = ConF
  frmExp (ConF f) = return f
  frmExp _        = badTypValM

instance (Rep a , Rep b) => Rep (a -> b) where
  toExp  f        = Abs (toExp . f . frmRgt . frmExp)
  frmExp (Abs f)  = return
                    (frmRgt . frmExp . f  . toExp)
  frmExp _        = badTypValM

instance (Rep a , Rep b) => Rep (a , b) where
  toExp (x , y)        = Tpl (toExp x , toExp y)
  frmExp (Tpl (x , y)) = ((,)) <$> frmExp x <*> frmExp y
  frmExp _             = badTypValM

instance Rep a => Rep (Array Word32 a) where
  toExp a        = Ary (fmap toExp a)
  frmExp (Ary x) = mapM frmExp x
  frmExp _       = badTypValM

instance Rep a => Rep (PH.Vec a) where
  toExp a        = Vec (fmap toExp a)
  frmExp (Vec v) = return (fmap (frmRgt . frmExp) v)
  frmExp _       = badTypValM

instance Rep (Complex Float) where
  toExp           = Cmx
  frmExp (Cmx c)  = return c
  frmExp _        = badTypValM

instance Rep a => Rep (Maybe a) where
  toExp x         = May (fmap toExp x)
  frmExp (May m)  = mapM frmExp m
  frmExp _        = badTypValM

prm0 :: Rep a => a -> NamM ErrM Exp
prm0 = return . toExp

prm1 :: (Rep a , Rep b) => (a -> b) -> Exp -> NamM ErrM Exp
prm1 f x = lift (do x' <- frmExp x
                    return (toExp (f x')))

prm2 :: (Rep a,Rep b,Rep c) =>
        (a -> b -> c) -> Exp -> Exp -> NamM ErrM Exp
prm2 f x y = lift (do x' <- frmExp x
                      y' <- frmExp y
                      return (toExp (f x' y')))

prm3 :: (Rep a,Rep b,Rep c,Rep d) =>
        (a -> b -> c -> d) -> Exp -> Exp -> Exp -> NamM ErrM Exp
prm3 f x y z = lift (do x' <- frmExp x
                        y' <- frmExp y
                        z' <- frmExp z
                        return (toExp (f x' y' z')))

var :: a -> NamM ErrM a
var = return

conI :: Word32 -> NamM ErrM Exp
conI =  prm0

conB :: Bool -> NamM ErrM Exp
conB =  prm0

conF :: Float -> NamM ErrM Exp
conF =  prm0

abs :: (Exp -> Exp) -> NamM ErrM Exp
abs f = return (Abs f)

app :: Exp -> Exp -> NamM ErrM Exp
app = prm2 ((\ f a -> f a) :: (Exp -> Exp) -> Exp -> Exp)

prm :: Exp -> [Exp] -> NamM ErrM Exp
prm = foldM app

cnd :: Exp -> Exp -> Exp -> NamM ErrM Exp
cnd = prm3 (PH.cnd :: Bool -> Exp -> Exp -> Exp)

whl :: Exp -> Exp -> Exp -> NamM ErrM Exp
whl = prm3 (PH.while :: (Exp -> Bool) -> (Exp -> Exp) -> Exp -> Exp)

fst :: Exp -> NamM ErrM Exp
fst =  prm1 (PH.fst :: (Exp , Exp) -> Exp)

snd :: Exp -> NamM ErrM Exp
snd =  prm1 (PH.snd :: (Exp , Exp) -> Exp)

tpl :: Exp -> Exp -> NamM ErrM Exp
tpl = prm2 ((,) :: Exp -> Exp -> (Exp , Exp))

ary :: Exp -> Exp -> NamM ErrM Exp
ary = prm2 (PH.mkArr :: Word32 -> (Word32 -> Exp) -> Array Word32 Exp)

len :: Exp -> NamM ErrM Exp
len = prm1 (PH.lnArr :: Array Word32 Exp -> Word32)

ind :: Exp -> Exp -> NamM ErrM Exp
ind  = prm2 (PH.ixArr :: Array Word32 Exp -> Word32 -> Exp)

cmx :: Exp -> Exp -> NamM ErrM Exp
cmx = prm2 ((PH.cmx) :: Float -> Float -> Complex Float)

leT :: Exp -> (Exp -> Exp) -> NamM ErrM Exp
leT e f = return (f e)

typ :: TA.Typ -> Exp -> NamM ErrM Exp
typ _ = return

mul :: Exp -> Exp -> NamM ErrM Exp
mul (ConI x) (ConI y) = return (toExp (x * y))
mul (ConF x) (ConF y) = return (toExp (x * y))
mul (Cmx  x) (Cmx  y) = return (toExp (x * y))
mul _        _        = badTypValM

add :: Exp -> Exp -> NamM ErrM Exp
add (ConI x) (ConI y) = return (toExp (x + y))
add (ConF x) (ConF y) = return (toExp (x + y))
add (Cmx  x) (Cmx  y) = return (toExp (x + y))
add _        _        = badTypValM

sub :: Exp -> Exp -> NamM ErrM Exp
sub (ConI x) (ConI y) = return (toExp (x - y))
sub (ConF x) (ConF y) = return (toExp (x - y))
sub (Cmx  x) (Cmx  y) = return (toExp (x - y))
sub _        _        = badTypValM

eql :: Exp -> Exp -> NamM ErrM Exp
eql (ConI x) (ConI y) = return (toExp (x == y))
eql (ConF x) (ConF y) = return (toExp (x == y))
eql (ConB x) (ConB y) = return (toExp (x == y))
eql _        _        = badTypValM

ltd :: Exp -> Exp -> NamM ErrM Exp
ltd (ConI x) (ConI y) = return (toExp (x < y))
ltd (ConF x) (ConF y) = return (toExp (x < y))
ltd (ConB x) (ConB y) = return (toExp (x < y))
ltd _        _        = badTypValM

int :: Word32 -> NamM ErrM Exp
int = prm0

mem :: Exp -> NamM ErrM Exp
mem = return

fix :: Exp -> NamM ErrM Exp
fix = prm1 (PH.fix :: (Exp -> Exp) -> Exp)

aryV :: Exp -> Exp -> NamM ErrM Exp
aryV = prm2 (PH.Vec :: Word32 -> (Word32 -> Exp) -> PH.Vec Exp)

lenV :: Exp -> NamM ErrM Exp
lenV = prm1 (PH.lnVec :: PH.Vec Exp -> Word32)

indV :: Exp -> Exp -> NamM ErrM Exp
indV = prm2 (PH.ixVec :: PH.Vec Exp -> Word32 -> Exp)

non :: NamM ErrM Exp
non  = prm0 (Nothing :: Maybe Exp)

som :: Exp -> NamM ErrM Exp
som  = prm1 (Just :: Exp -> Maybe Exp)

may :: Exp -> Exp -> Exp -> NamM ErrM Exp
may = prm3 (PH.may ::  Maybe Exp -> Exp -> (Exp -> Exp) -> Exp)
