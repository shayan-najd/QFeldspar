module QFeldspar.Solver where

import QFeldspar.MyPrelude hiding (mem)

import QFeldspar.Singleton
import QFeldspar.Environment.Scoped
import QFeldspar.Type.Herbrand

slv :: [Typ r] -> [HerCon r] -> ErrM [Typ r]
slv mem []
  = return mem
slv mem ((App c ts :~: App c' ts') : cs)
  = do Rfl <- eqlSin (len ts) (len ts')
       if c == c'
         then slv mem  (zipWith (:~:) (toList ts) (toList ts') ++ cs)
         else fail "Type Error!"
slv mem ((Mta i :~: Mta j) : cs) | i == j
  = slv mem cs
slv mem ((Mta i :~: t) : cs) | notElem i (mtas t)
  = slv (appTs i t mem) (appCs i t cs)
slv mem ((t :~: Mta i) : cs) | notElem i (mtas t)
  = slv (appTs i t mem) (appCs i t cs)
slv _   ((_ :~: _ ) : _)
  = fail "Type Error!"
