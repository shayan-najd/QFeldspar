{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Utils.GADTTyped
       (sucAll,prdAll,mapVar,sbs,fre,fre') where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTTyped as GT
import QFeldspar.Variable.Scoped
import qualified QFeldspar.Nat.ADT as NA

sucAll :: Exp n t -> Exp (NA.Suc n) t
sucAll = mapVar Suc

prdAll :: Exp (NA.Suc n) t -> Exp n t
prdAll = mapVar prd

mapVar :: forall n n' t. (Var n -> Var n') -> Exp n t -> Exp n' t
mapVar f ee = case ee of
  Var v  -> Var (f v)
  _      -> $(genOverloaded 'ee ''Exp  ['Var]
   (\ t -> if
    | matchQ t [t| Exp (NA.Suc t) t |] -> [| mapVar (inc f) |]
    | matchQ t [t| Exp t t |]          -> [| mapVar f |]
    | otherwise                        -> [| id |]))

sbs :: forall n t. Exp n t -> Var n -> Exp n t -> Exp n t
sbs ee v ea = case ee of
  Var x
    | x == v     -> ea
    | otherwise  -> ee
  _  -> $(genOverloaded 'ee ''Exp ['Var]
   (\ t -> if
    | matchQ t [t| Exp (NA.Suc t) t |] ->
        [| \ e -> sbs e (Suc v) (sucAll ea) |]
    | matchQ t [t| Exp t t |]          ->
        [| \ e -> sbs e v ea |]
    | otherwise                        ->
        [| id |]))

fre :: Exp (NA.Suc n) t -> [Var (NA.Suc n)]
fre = fre' Zro

fre' :: forall n t. Var n -> Exp n t -> [Var n]
fre' v ee = case ee of
 Var x
  | x >= v    -> [x]
  | otherwise -> []
 _            -> $(recAppMQ 'ee ''Exp (const [| [] |]) ['Var]
    [| \ _x -> [] |] [| (++) |] [| (++) |] (const id)
  (\ t -> if
   | matchQ t [t| Exp (NA.Suc t) t |] ->  [| fmap prd . fre' (Suc v) |]
   | matchQ t [t| Exp t t |]          ->  [| fre' v |]
   | otherwise                        ->  [| \ _x -> [] |]))
