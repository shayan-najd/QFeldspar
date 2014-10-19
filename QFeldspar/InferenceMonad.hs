module QFeldspar.InferenceMonad where

import QFeldspar.MyPrelude

import QFeldspar.Type.Herbrand
import QFeldspar.Nat.ADT

type InfM r a = State (Nat , [HerCon r]) a

newMT :: InfM r (Typ r)
newMT = do (i , x) <- getState
           put (Suc i , x)
           return (Mta i)

addC  :: HerCon r -> InfM r ()
addC c = modify (\ (i , cs) -> (i , c : cs))

