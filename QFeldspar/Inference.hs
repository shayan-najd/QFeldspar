module QFeldspar.Inference where

import QFeldspar.MyPrelude

import qualified QFeldspar.TypeChecking as Chk
import QFeldspar.Nat.ADT
import QFeldspar.Type.Herbrand
import QFeldspar.Solver
import QFeldspar.Conversion
import QFeldspar.Nat.Conversion ()

mxm :: [Nat] -> Nat
mxm [] = Zro
mxm l  = maximum l

ind :: Traversable ef =>
       ef (Maybe (Typ r)) -> ef (Typ r)
ind e = (flip evalState  (succ (maxMta (fmap (maybe (Mta Zro) id) e))) .
              mapM (maybe (do i <- getState
                              put (Suc i)
                              return (Mta i)) return)) e

typInf :: (Chk.Chk ef , Traversable ef , r ~ Chk.Cns ef) =>
       ef (Maybe (Typ r)) -> (Chk.Env ef) (Typ r) -> ErrM (ef (Typ r))
typInf e r = inf (ind e) r

maxMta :: Foldable f => f (Typ r) -> Nat
maxMta = mxm . concat . fmap mtas . toList

inf :: (Chk.Chk ef , Traversable ef , r ~ Chk.Cns ef) =>
       ef (Typ r) -> (Chk.Env ef) (Typ r) -> ErrM (ef (Typ r))
inf e r = do let mts = (mxm . concat) [mtas x | x <- toList e]
                 (i' , cs) = execState (Chk.chk e r) (succ mts , [])
             mTs <- slv (fmap Mta [Zro .. pred i']) cs
             let ttas = zip [Zro ..] mTs

             return (fmap (appTtas ttas) e)

chk :: (Chk.Chk ef , Traversable ef , r ~ Chk.Cns ef) =>
       ef (Typ r) -> (Chk.Env ef) (Typ r) -> ErrM (Typ r)
chk e r = do let mts = (mxm . concat) [mtas x | x <- toList e]
                 (t , (i' , cs)) = runState (Chk.chk e r) (mts , [])
             mTs <- slv (fmap Mta [Zro .. pred i']) cs
             let ttas = zip [Zro ..] mTs
             if length [() | Mta _ <- mTs] == 0
               then return (appTtas ttas t)
               else fail "Type Error!"

typChk :: forall ef t.
          (Chk.Chk ef , Traversable ef, Cnv (t , ()) (Typ (Chk.Cns ef))
          , Traversable (Chk.Env ef)
          , Cnv (Typ (Chk.Cns ef), ()) t)  =>
          ef t -> Chk.Env ef t-> ErrM t
typChk e r = do r' :: (Chk.Env ef) (Typ (Chk.Cns ef)) <- traverse
                                                         (flip (curry cnv) ()) r
                e' :: ef  (Typ (Chk.Cns ef)) <- traverse (flip (curry cnv) ()) e
                t <- chk e' r'
                cnv (t , ())