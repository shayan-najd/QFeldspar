module QFeldspar.Expression.Utils.ADTUntypedNamed
       (sbs)  where

import QFeldspar.MyPrelude
import QFeldspar.Expression.ADTUntypedNamed

sbs :: forall x. Eq x => x -> Exp x -> Exp x -> Exp x
sbs xx ee' ee = case ee of
  Var  y
     | y == xx    -> ee'
     | otherwise  -> ee
  _               -> $(genOverloaded 'ee ''Exp ['Var]
   (\ t -> if
    | matchQ t [t| [Exp x] |]     -> [| fmap (sbs xx ee') |]
    | matchQ t [t| (x , Exp x) |] -> [| sbsF xx ee' |]
    | matchQ t [t| Exp x |]       -> [| sbs  xx ee' |]
    | otherwise                   -> [| id |]))

sbsF :: Eq x => x -> Exp x -> (x , Exp x) -> (x , Exp x)
sbsF x e' (y , e)
    | y == x    = (y , e)
    | otherwise = (y , sbs x e' e)
