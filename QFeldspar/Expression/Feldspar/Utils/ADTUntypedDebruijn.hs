{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Feldspar.Utils.ADTUntypedDebruijn
       (fre) where

import QFeldspar.MyPrelude
import QFeldspar.Expression.Feldspar.ADTUntypedDebruijn
import QFeldspar.Variable.Plain

fre :: Exp -> [Nat]
fre ee = case ee of
  Var  n        -> [n]
  _             -> $(recAppMQ 'ee ''Exp (const [| [] |]) ['Var]
                   [| \ _x -> [] |] [| (++) |] [| (++) |] (const id)
   (\ t -> if
    | matchQ t [t| Fun |] -> [| freF |]
    | matchQ t [t| Exp |] -> [| fre  |]
    | otherwise           -> [| \ _x -> [] |]))

freF :: Fun -> [Nat]
freF (Fun f) = (fmap prd . filter (/= Zro) . fre) f
