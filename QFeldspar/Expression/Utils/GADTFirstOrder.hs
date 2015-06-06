{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Utils.GADTFirstOrder
       (sucAll,prdAll,prdAllM,mapVar,sbs,replaceOne,cntVar,pattern TF
       ,isVal,val,pattern V,pattern NV) where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTFirstOrder as GFO
import QFeldspar.Variable.Typed
import QFeldspar.Expression.Utils.Common
import QFeldspar.Singleton
import QFeldspar.Type.GADT hiding (May,Cmx,Ary,Tpl)
import qualified QFeldspar.Environment.Typed as ET


tagFree :: Exp s g a -> Exp s g a
tagFree (Tag _ e) = tagFree e
tagFree e         = e

pattern TF e <- (tagFree -> e)


sucAll :: Exp s g b -> Exp s (a ': g) b
sucAll = mapVar Suc

prdAll :: Exp s (a ': g) b -> Exp s g b
prdAll = mapVar (\(Suc x) -> x)

prdAllM :: Exp s (a ': g) b -> Maybe (Exp s g b)
prdAllM = mapVarM (\ v -> case v of
                            Zro -> Nothing
                            Suc x -> Just x)


mapVarM :: forall s g g' a m. (Applicative m , Monad m) =>
           (forall b. Var g b -> m (Var g' b)) -> Exp s g a -> m (Exp s g' a)
mapVarM f ee = case ee of
 Var x    -> Var <$> f x
 Prm x ns -> Prm x <$> ET.mapM (mapVarM f) ns
 _        -> $(genOverloadedM 'ee ''Exp  ['Prm, 'Var]
  (\ t -> if
      | matchQ t [t| Exp a (a ': a) a |] -> [| mapVarM (incM f) |]
      | matchQ t [t| Exp a a a |]        -> [| mapVarM f  |]
      | otherwise                        -> [| pure |]))

mapVar :: forall s g g' a.
          (forall b. Var g b -> Var g' b) -> Exp s g a -> Exp s g' a
mapVar f ee = case ee of
 Var x    -> Var (f x)
 Prm x ns -> Prm x (ET.fmap (mapVar f) ns)
 _        -> $(genOverloaded 'ee ''Exp  ['Prm,'Var]
  (\ t -> if
      | matchQ t [t| Exp a (a ': a) a |] -> [| mapVar (inc f) |]
      | matchQ t [t| Exp a a a |]        -> [| mapVar f  |]
      | otherwise                        -> [| id |]))


sbs :: (HasSin Typ a, HasSin Typ b) =>
       Exp s g a -> Exp s (a ': g) b -> Exp s g b
sbs e' ee = prdAll (sbs' (sucAll e') Zro ee)

rp :: Var (a ': g) t -> Var (a ': b ': g) t
rp v = case v of
         Zro    -> Zro
         Suc v' -> Suc (Suc v')

replaceOne :: Exp s (a ': g) b -> Exp s (a ': c ': g) b
replaceOne = mapVar rp

cntVar :: forall s g a b. (HasSin Typ a , HasSin Typ b) =>
          Var g b -> Exp s g a -> Word32
cntVar v ee = let t = sin :: Typ a in case ee of
  Var x     -> case eqlSin t (sinTyp v) of
    Rgt Rfl -> if x == v
               then 1
               else 0
    _       -> 0
  _         -> $(recAppMQ 'ee ''Exp (const [| (0 :: Word32) |]) ['Var]
    [| \ _x -> 0 |] [| (+) |] [| (+) |] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| Exp a (a ': a) a |] -> [| cntVar (Suc v) |]
    | matchQ tt [t| ET.Env (Exp a a)  a |] -> [| fld (\ b e -> b + cntVar v e) 0 |]
    | matchQ tt [t| Exp a a a |]        -> [| cntVar v |]
    | otherwise                         -> [| const 0  |]))

sbs' :: forall s g a b.
       (HasSin Typ a , HasSin Typ b) =>
       Exp s g b -> Var g b -> Exp s g a -> Exp s g a
sbs' e' v' ee = let t = sin :: Typ a in case ee of
  Var v -> case eqlSin t (sinTyp v') of
     Rgt Rfl -> if v == v'
                then e'
                else ee
     _       -> ee
  Prm x ns   -> Prm x (mapC (sbs' e' v') ns)
  _   -> $(genOverloadedW 'ee ''Exp  ['Var] (trvWrp 't)
   (\ tt -> if
      | matchQ tt [t| Exp a (a ': a) a |] -> [| sbs'F e' v' |]
      | matchQ tt [t| Exp a a a |]        -> [| sbs'  e' v' |]
      | otherwise                         -> [| id |]))

sbs'F :: forall s g a b c.
        (HasSin Typ a, HasSin Typ b) =>
        Exp s g b -> Var g b -> Exp s (c ': g) a -> Exp s (c ': g) a
sbs'F e' v' e = sbs' (sucAll e') (Suc v') e

isVal :: Exp s g a -> Bool
isVal ee = case ee of
    ConI _        -> True
    ConB _        -> True
    ConF _        -> True
    Prm  _ _      -> False
    Var  _        -> True
    Abs  _        -> True
    App  _  _     -> False
    Cnd  _  _  _  -> False
    Whl  _  _  _  -> False
    Tpl  ef es    -> isVal ef && isVal es
    Fst  _        -> False
    Snd  _        -> False
    Ary  el  _    -> isVal el
    Len  _        -> False
    Ind  _  _     -> False
    AryV el  _    -> isVal el
    LenV  _       -> False
    IndV  _  _    -> False
    LeT  _  _     -> False
    Cmx  _  _     -> True
    Non           -> True
    Som  e        -> isVal e
    May  _ _  _   -> False
    Mul _ _       -> False
    Add _ _       -> False
    Sub _ _       -> False
    Eql _ _       -> False
    Ltd _ _       -> False
    Int _         -> True -- shouldn't matter
    Tag _ e       -> isVal e
    Mem _         -> False
    Fix _         -> False

val :: Exp s g a -> (Bool , Exp s g a)
val ee = (isVal ee , ee)

pattern V  v <- (val -> (True  , v))
pattern NV v <- (val -> (False , v))
