module QFeldspar.Prelude.TemplateHaskell
       (Data
       ,Int --,litI
       ,Flt --,litF
       ,Bol,Bool(True,False)
       ,fst,snd
       ,Cmx,cmx,real,imag
       ,Ary,ary,len,ind
       ,{-ifThenElse,-}whl,forLoop,memorize
       ,not,and,or
       ,Equality(eql),notEql
       ,Ordering(lt),gt,lte,gte,min
       ,Numeric(add,sub,mul,div,neg),ilog2,pi,sqrt
       ,bitXor,bitAnd,bitOr,shfRgt,shfLft,complement,testBit,lsbs,oneBits
       ,i2f,cis
       ,frmTo,permute,reverse,foldl,map,zipWith,sum,scalarProd,fromList
       ,replicate,append,hashTable,FO
       ) where

import Prelude (toRational)

import QFeldspar.MyPrelude (Ary,Flt,Bol,Bool(True,False),Int,Cmx)
import qualified QFeldspar.MyPrelude as MP

import Language.Haskell.TH.Syntax (Lift(lift),Q,Exp(LitE),TExp
                                  ,Lit(IntegerL,RationalL))

import QFeldspar.Prelude.Environment
import qualified QFeldspar.VanillaPrelude as VP

type Data t = Q (TExp t)

class    FO a                           where {}
instance FO Bol                         where {}
instance FO Int                         where {}
instance FO Flt                         where {}
instance (FO a , FO b) => FO (a , b)    where {}
instance FO a => FO (Ary a)             where {}
instance FO Cmx                         where {}

instance Lift Int where
  lift i = MP.return (LitE (IntegerL (MP.toInteger i)))

instance Lift Flt where
  lift f = MP.return (LitE (RationalL (toRational f)))

---------------------------------------------------------------------------------
-- Tuple
---------------------------------------------------------------------------------

fst :: (a , b) -> a
fst = VP.fst

snd :: (a , b) -> b
snd = VP.snd

---------------------------------------------------------------------------------
-- Complex
---------------------------------------------------------------------------------

cmx :: Flt -> Flt -> Cmx
cmx = VP.cmx

real :: Data (Cmx -> Flt)
real = [|| \ e -> realPartHsk e ||]

imag :: Data (Cmx -> Flt)
imag = [|| \ e -> imagPartHsk e ||]

---------------------------------------------------------------------------------
-- Ary
---------------------------------------------------------------------------------

ary :: FO a => Int -> (Int -> a) -> Ary a
ary = VP.ary

len :: Ary a -> Int
len = VP.len

ind :: Ary a -> Int -> a
ind = VP.ind

---------------------------------------------------------------------------------
-- Control Flow
---------------------------------------------------------------------------------

whl :: FO s => (s -> Bol) -> (s -> s) -> s -> s
whl = VP.whl

forLoop :: FO s => Data (Int -> s -> (Int -> s -> s) -> s)
forLoop = [|| \ l -> \ init -> \ step ->
              snd (whl (\ t  -> $$lt (fst t) l)
                       (\ tt -> let t  = tt    in
                                let ft = fst t in
                                ( $$add ft 1
                                , step ft (snd t)))
                   (0 , init))
          ||]

memorize :: Data (Ary Flt -> Ary Flt)
memorize = [|| memHsk ||]

---------------------------------------------------------------------------------
-- Boolean Operators
---------------------------------------------------------------------------------

not :: Data (Bol -> Bol)
not = [|| \ x -> if x then False else True ||]

and :: Data (Bol -> Bol -> Bol)
and = [|| \ x -> \ y -> if x then y else False ||]

or :: Data (Bol -> Bol -> Bol)
or = [|| \ x -> \ y -> if x then True else y ||]

---------------------------------------------------------------------------------
-- Equality
---------------------------------------------------------------------------------

class Equality t where
  eql :: Data (t -> t -> Bol)

instance Equality Bol where
  eql = [|| \ x -> \ y -> eqlBolHsk x y ||]

instance Equality Int where
  eql = [|| \ x -> \ y -> eqlIntHsk x y ||]

instance Equality Flt where
  eql = [|| \ x -> \ y -> eqlFltHsk x y ||]

notEql :: Equality t => Data (t -> t -> Bol)
notEql= [|| \ x -> \ y -> $$not ($$eql x y) ||]

---------------------------------------------------------------------------------
-- Ordering
---------------------------------------------------------------------------------

class Ordering t where
  lt :: Data (t -> t -> Bol)

instance Ordering Bol where
  lt = [|| \ x -> \ y -> ltdBolHsk x y ||]

instance Ordering Int where
  lt = [|| \ x -> \ y -> ltdIntHsk x y ||]

instance Ordering Flt where
  lt = [|| \ x -> \ y -> ltdFltHsk x y ||]

gt :: (Equality t , Ordering t) => Data (t -> t -> Bol)
gt = [|| \ xx -> \ yy -> let x = xx in
                         let y = yy in
                         $$not ($$or ($$lt x y) ($$eql x y)) ||]

lte :: (Equality t , Ordering t) => Data (t -> t -> Bol)
lte = [|| \ xx -> \ yy -> let x = xx in
                          let y = yy in
                          $$or ($$lt x y) ($$eql x y) ||]

gte :: (Equality t , Ordering t) => Data (t -> t -> Bol)
gte = [|| \ x -> \ y -> $$not ($$lt x y) ||]

min :: Ordering t => Data(t -> t -> t)
min = [|| \ xx -> \ yy -> let x = xx in
                          let y = yy in
                          if ($$lt x y) then x else y ||]

---------------------------------------------------------------------------------
-- Numeric
---------------------------------------------------------------------------------

class Numeric t where
  add :: Data (t -> t -> t)
  sub :: Data (t -> t -> t)
  mul :: Data (t -> t -> t)
  div :: Data (t -> t -> t)
  neg :: Data (t -> t)

instance Numeric Int where
  add = [|| addIntHsk ||]
  sub = [|| subIntHsk ||]
  mul = [|| mulIntHsk ||]
  div = [|| divIntHsk ||]
  neg = [|| \ i -> $$sub 0 i ||]

instance Numeric Flt where
  add = [|| addFltHsk ||]
  sub = [|| subFltHsk ||]
  mul = [|| mulFltHsk ||]
  div = [|| divFltHsk ||]
  neg = [|| \ f -> $$sub 0.0 f ||]

instance Numeric (Cmx) where
  add = [|| addCmxHsk ||]
  sub = [|| subCmxHsk ||]
  mul = [|| mulCmxHsk ||]
  div = [|| divCmxHsk ||]
  neg = [|| \ c -> $$sub (cmx 0.0 0.0) c ||]

ilog2 :: Data (Int -> Int)
ilog2 = [|| ilog2Hsk ||]
  {-
  [|| \ xx -> ($$((-))) 31  ($$nlz xx) ||]
 where
   nlz :: Data (Int -> Int)
   nlz = [|| \ x -> $$bitCount ($$complement
                                $$(MP.foldl go [|| x ||] [1,2,4,8,16])) ||]
     where
       go :: Data Int -> Int -> Data Int
       go b s = [|| $$((.|.)) $$b  ($$((.>>.)) $$b s) ||]
   -}

pi :: Data Flt
pi =  let p = (MP.negate MP.pi) :: MP.Flt
      in  [|| p ||]

sqrt :: Data (Flt -> Flt)
sqrt = [|| sqrtFltHsk ||]

---------------------------------------------------------------------------------
-- Bitwise Operators
---------------------------------------------------------------------------------

bitXor :: Data (Int -> Int -> Int)
bitXor = [|| \ el -> \ er -> xorIntHsk el er ||]

bitAnd :: Data (Int -> Int -> Int)
bitAnd = [|| \ el -> \ er -> andIntHsk el er ||]

bitOr  :: Data (Int -> Int -> Int)
bitOr  = [|| \ el -> \ er -> orIntHsk el er ||]

shfRgt :: Data (Int -> Int -> Int)
shfRgt = [|| \ el -> \ er -> shrIntHsk el er ||]

shfLft :: Data (Int -> Int -> Int)
shfLft = [|| \ el -> \ er -> shlIntHsk el er ||]

complement :: Data (Int -> Int)
complement = [|| \ e -> cmpIntHsk e ||]

testBit    :: Data (Int -> Int -> Bol)
testBit    = [|| \ i -> \ j -> if $$eql ($$bitAnd i ($$shfLft 1 j)) 0
                                  then False
                                  else True ||]

oneBits :: Data (Int -> Int)
oneBits    =  [|| \ n -> $$complement ($$shfLft ($$complement 0) n) ||]

lsbs :: Data (Int -> Int -> Int)
lsbs       = [|| \ k -> \ i -> $$bitAnd i ($$oneBits k) ||]

---------------------------------------------------------------------------------
-- Conversion Operators
---------------------------------------------------------------------------------

i2f :: Data (Int -> Flt)
i2f = [|| \ e -> i2fHsk e ||]

cis :: Data (Flt -> Cmx)
cis = [|| \ e -> cisHsk e ||]

---------------------------------------------------------------------------------
-- Array Operators
---------------------------------------------------------------------------------

frmTo :: Data (Int -> Int -> Ary Int)
frmTo = [|| \ mm -> \ nn -> let n = nn in
                            let m = mm in
                            ary
                            (if ($$lt n m)
                             then 0
                             else ($$add ($$sub n m) 1))
                            (\ i -> $$add i m) ||]

permute :: FO t => Data ((Int -> Int -> Int) -> Ary t -> Ary t)
permute = [|| \ f -> \ v -> let lv = len v in
                            ary lv (\ i -> ind v (f lv i)) ||]

reverse :: FO t => Data (Ary t -> Ary t)
reverse = [|| $$permute (\ l -> \ i -> $$sub ($$sub l 1) i) ||]

foldl :: (FO a , FO b) => Data ((a -> b -> a) -> a -> Ary b -> a)
foldl = [|| \ f -> \ acc -> \ v ->
            $$forLoop (len v) acc (\ i -> \ a -> f a (ind v i)) ||]

map :: (FO a , FO b) => Data ((a -> b) -> Ary a -> Ary b)
map = [|| \ f -> \ v -> ary (len v) (\ i -> f (ind v i)) ||]

zipWith :: (FO a , FO b , FO c) =>
           Data ((a -> b -> c) -> Ary a -> Ary b -> Ary c)
zipWith = [|| \ f -> \ v1 -> \ v2 ->
                ary ($$min (len v1) (len v2))
                    (\ ii -> let i = ii in
                             f (ind v1 i) (ind v2 i)) ||]

sum :: Data (Ary Int -> Int)
sum = [|| $$foldl $$add 0 ||]

scalarProd :: Data (Ary Int -> Ary Int -> Int)
scalarProd  = [|| \ v1 -> \ v2 -> $$sum ($$zipWith $$mul v1 v2) ||]

replicate :: FO a => Data (Int -> a -> Ary a)
replicate = [|| \ n -> \ x -> ary n (\ _i -> x) ||]

append :: FO a => Data (Ary a -> Ary a -> Ary a)
append = [|| \ a1 -> \ a2 -> let la1 = len a1 in
                             ary ($$add la1 (len a2))
                                 (\ ii -> let i = ii in
                                          if $$lt i la1
                                          then ind a1 i
                                          else ind a2 i) ||]

fromList :: FO a => [Data a] -> Data a -> Data (Ary a)
fromList lst k =  let l = MP.fromInteger (MP.toInteger (MP.length lst))
                  in  [|| ary l
                          (\ i -> $$(MP.foldr
                                     (\ j acc ->
                                       let l' = (MP.fromInteger (MP.toInteger j))
                                       in  [|| if   $$eql i l'
                                               then $$(lst MP.!! j)
                                               else $$acc ||]) k
                                      (MP.enumFromTo 0 (MP.length lst MP.- 1))
                                     )
                          )
                      ||]

hashTable :: Data (Ary Int)
hashTable = [|| hshTblHsk ||]

