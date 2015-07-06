module QFeldspar.Expression.Utils.GADTTyped
       (sucAll,prdAll,mapVar,sbs,fre,fre') where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTTyped as GT
import QFeldspar.Variable.Scoped
import qualified QFeldspar.Nat.ADT as NA

sucAll :: Exp m n t -> Exp m (NA.Suc n) t
sucAll = mapVar Suc

prdAll :: Exp m (NA.Suc n) t -> Exp m n t
prdAll = mapVar prd

mapVar :: forall m n n' t. (Var n -> Var n') -> Exp m n t -> Exp m n' t
mapVar f ee = case ee of
  Var v  -> Var (f v)
  _      -> $(genOverloaded 'ee ''Exp  ['Var]
   (\ t -> if
    | matchQ t [t| Exp t (NA.Suc t) t |] -> [| mapVar (inc f) |]
    | matchQ t [t| Exp t t t |]          -> [| mapVar f |]
    | matchQ t [t| [Exp t t t] |]        -> [| fmap (mapVar f) |]
    | otherwise                          -> [| id |]))

sbs :: forall m n t. Exp m n t -> Var n -> Exp m n t -> Exp m n t
sbs ee v ea = case ee of
  Var x
    | x == v     -> ea
    | otherwise  -> ee
  _  -> $(genOverloaded 'ee ''Exp ['Var]
   (\ t -> if
    | matchQ t [t| Exp t (NA.Suc t) t |] ->
        [| \ e -> sbs e (Suc v) (sucAll ea) |]
    | matchQ t [t| Exp t t t |]          ->
        [| \ e -> sbs e v ea |]
    | matchQ t [t| [Exp t t t] |]        ->
        [| fmap (\ e -> sbs e v ea) |]
    | otherwise                          ->
        [| id |]))

fre :: Exp m (NA.Suc n) t -> [Var (NA.Suc n)]
fre = fre' Zro

fre' :: forall m n t. Var n -> Exp m n t -> [Var n]
fre' v ee = case ee of
 Var x
  | x >= v    -> [x]
  | otherwise -> []
 _            -> $(recAppMQ 'ee ''Exp (const [| [] |]) ['Var]
    [| \ _x -> [] |] [| (++) |] [| (++) |] (const id)
  (\ t -> if
   | matchQ t [t| Exp t (NA.Suc t) t |] ->  [| fmap prd . fre' (Suc v) |]
   | matchQ t [t| Exp t t t |]          ->  [| fre' v |]
   | matchQ t [t| [Exp t t t] |]        ->  [| concat . fmap (fre' v) |]
   | otherwise                          ->  [| \ _x -> [] |]))
