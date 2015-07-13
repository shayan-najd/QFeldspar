module QFeldspar.Expression.Utils.ADTUntypedDebruijn
       (fre) where

import QFeldspar.MyPrelude
import QFeldspar.Expression.ADTUntypedDebruijn
import QFeldspar.Variable.Plain

fre :: Exp -> [Nat]
fre ee = case ee of
  Var  n        -> [n]
  _             -> $(recAppMQ 'ee ''Exp (const [| [] |]) ['Var]
                   [| \ _x -> [] |] [| (++) |] [| (++) |] (const id)
   (\ t -> if
    | matchQ t [t| [Exp] |]  -> [| concatMap fre  |]
    | matchQ t [t| Fun |] -> [| freF |]
    | matchQ t [t| Exp |] -> [| fre  |]
    | otherwise           -> [| \ _x -> [] |]))

freF :: Fun -> [Nat]
freF (Fun f) = (fmap prd . filter (/= Zro) . fre) f
