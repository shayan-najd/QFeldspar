module QFeldspar.Expression.Feldspar.ADTUntypedNamed
    (Exp(..),sbs) where

import QFeldspar.MyPrelude
import qualified QFeldspar.Type.Feldspar.ADT as TFA

data Exp x = ConI Int
           | ConB Bool
           | ConF Float
           | Var x
           | Abs (x , Exp x)
           | App (Exp x) (Exp x)
           | Cnd (Exp x) (Exp x) (Exp x)
           | Whl (x , Exp x) (x , Exp x) (Exp x)
           | Tpl (Exp x) (Exp x)
           | Fst (Exp x)
           | Snd (Exp x)
           | Ary (Exp x) (x , Exp x)
           | Len (Exp x)
           | Ind (Exp x) (Exp x)
           | AryV (Exp x) (x , Exp x)
           | LenV (Exp x)
           | IndV (Exp x) (Exp x)
           | Let (Exp x) (x , Exp x)
           | Cmx (Exp x) (Exp x)
           | Non
           | Som (Exp x)
           | May (Exp x) (Exp x) (x , Exp x)
           | Typ TFA.Typ (Exp x)

deriving instance Eq x   => Eq   (Exp x)
deriving instance Show x => Show (Exp x)
deriving instance Functor     Exp
deriving instance Foldable    Exp
deriving instance Traversable Exp

sbs :: Eq x => x -> Exp x -> Exp x -> Exp x
sbs x e' ee = case ee of
  ConI i         -> ConI i
  ConB b         -> ConB b
  ConF f         -> ConF f
  Var  y
   | y == x      -> e'
   | otherwise   -> ee
  Abs xeb        -> Abs (sf xeb)
  App ef ea      -> App (s ef) (s ea)
  Cnd ec et ef   -> Cnd (s ec) (s et) (s ef)
  Whl xec xeb ei -> Whl (sf xec) (sf xeb) (s ei)
  Tpl ef es      -> Tpl (s ef) (s es)
  Fst e          -> Fst (s e)
  Snd e          -> Snd (s e)
  Ary el xei     -> Ary (s el) (sf xei)
  Len e          -> Len (s e)
  Ind e ei       -> Ind (s e)  (s ei)
  AryV el xei    -> AryV (s el) (sf xei)
  LenV e         -> LenV (s e)
  IndV e ei      -> IndV (s e)  (s ei)
  Let ei xeb     -> Let (s ei) (sf xeb)
  Cmx er ei      -> Cmx (s er) (s ei)
  Non            -> Non
  Som e          -> Som (s e)
  May em en xej  -> May (s em) (s en) (sf xej)
  Typ t e        -> Typ t (s e)
  where
    s = sbs x e'
    sf (y , e)
      | y == x    = (y , e)
      | otherwise = (y , s e)
