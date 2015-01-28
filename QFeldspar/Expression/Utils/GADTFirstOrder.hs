{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Utils.GADTFirstOrder
       (sucAll,prdAll,mapVar,sbs,replaceOne,isFresh) where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTFirstOrder as FGFO
import QFeldspar.Variable.Typed
import QFeldspar.Expression.Utils.Common
import QFeldspar.Singleton
import QFeldspar.Type.GADT

sucAll :: Exp r t' -> Exp (t ': r) t'
sucAll = mapVar Suc

prdAll :: Exp (t ': r) t' -> Exp r t'
prdAll = mapVar (\(Suc x) -> x)

mapVar :: forall r r' t.
          (forall t'. Var r t' -> Var r' t') -> Exp r t -> Exp r' t
mapVar f ee = case ee of
 Var v -> Var (f v)
 _     -> $(genOverloaded 'ee ''Exp  ['Var]
  (\ t -> if
      | matchQ t [t| Exp (t ': t) t |] -> [| mapVar (inc f) |]
      | matchQ t [t| Exp t t |]        -> [| mapVar f  |]
      | otherwise                      -> [| id |]))

deriving instance Show (Exp r t)

sbs :: (HasSin Typ a, HasSin Typ b) =>
       Exp r a -> Exp (a ': r) b -> Exp r b
sbs e' ee = prdAll (sbs' (sucAll e') Zro ee)

rp :: Var (a ': g) t -> Var (a ': b ': g) t
rp v = case v of
         Zro    -> Zro
         Suc v' -> Suc (Suc v')

replaceOne :: Exp (tl ': n) t -> Exp (tl ': tl1 ': n) t
replaceOne = mapVar rp

isFresh :: (HasSin Typ a , HasSin Typ b) =>
           Exp (a ': g) b -> Bool
isFresh = (not . hasVar Zro)

hasVar :: forall r t t'.
          (HasSin Typ t' , HasSin Typ t) =>
          Var r t' -> Exp r t -> Bool
hasVar v ee = let t = sin :: Typ t in case ee of
  Var x -> case eqlSin t (sinTyp v) of
     Rgt Rfl -> x == v
     _       -> False
  _             -> $(recAppMQ 'ee ''Exp (const [| False |]) ['Var]
    [| \ _x -> False |] [| (||) |] [| (||) |] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| Exp (t ': t) t |] -> [| hasVar (Suc v) |]
    | matchQ tt [t| Exp t t |]        -> [| hasVar v       |]
    | otherwise                       -> [| const False |]))

sbs' :: forall r t t'.
       (HasSin Typ t , HasSin Typ t') =>
       Exp r t' -> Var r t' -> Exp r t -> Exp r t
sbs' e' v' ee = let t = sin :: Typ t in case ee of
  Var v -> case eqlSin t (sinTyp v') of
     Rgt Rfl -> if v == v'
                then e'
                else ee
     _       -> ee
  _   -> $(genOverloadedW 'ee ''Exp  ['Var] (trvWrp 't)
   (\ tt -> if
      | matchQ tt [t| Exp (t ': t) t |] -> [| sbs'F e' v' |]
      | matchQ tt [t| Exp t t |]        -> [| sbs'  e' v' |]
      | otherwise                       -> [| id |]))

sbs'F :: forall r t t' t''.
        (HasSin Typ t', HasSin Typ t) =>
        Exp r t' -> Var r t' -> Exp (t'' ': r) t -> Exp (t'' ': r) t
sbs'F e' v' e = sbs' (sucAll e') (Suc v') e
