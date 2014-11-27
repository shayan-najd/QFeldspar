{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Utils.GADTFirstOrder
       (sucAll,prdAll,mapVar) where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTFirstOrder as FGFO
import QFeldspar.Variable.Typed

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
