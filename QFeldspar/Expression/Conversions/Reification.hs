module QFeldspar.Expression.Conversions.Reification(rei,reiVar)  where

import QFeldspar.Variable.Typed                             as VT
import QFeldspar.Environment.Typed
import qualified QFeldspar.Expression.GADTFirstOrder        as GFO
import QFeldspar.Variable.Conversion ()
import qualified QFeldspar.Literal.GADT as LG

import qualified Language.Haskell.TH.Syntax as TH

reiVar :: Var g a -> TH.Q (TH.TExp (Var g a))
reiVar x = [|| x ||]

rei :: GFO.Exp s g a -> TH.Q (TH.TExp (GFO.Exp s g a))
rei ee = [|| ee ||]

instance TH.Lift (Var g a) where
 lift x = case x of
  Zro   -> [| Zro |]
  Suc y -> [| Suc y |]

instance TH.Lift (Env (GFO.Exp s g) d) where
 lift g = case g of
   Emp      -> [| Emp |]
   Ext x xs -> [| Ext x xs |]

instance TH.Lift (LG.Lit a) where
  lift l = case l of
    LG.IntegerL  i -> [| LG.IntegerL i |]
    LG.RationalL i -> [| LG.RationalL i |]
    LG.CharL     i -> [| LG.CharL i |]
    LG.StringL   i -> [| LG.StringL i |]

instance TH.Lift (GFO.Exp s g a) where
 lift ee = case ee of
  GFO.Lit  i    -> [| GFO.Lit i |]
  GFO.ConB b    -> [| GFO.ConB b |]
  GFO.Prm x ms  -> [| GFO.Prm x ms |]
  GFO.Cnd l m n -> [| GFO.Cnd  l m n |]
  GFO.Whl l m n -> [| GFO.Whl l m n |]
  GFO.Var x     -> [| GFO.Var  x |]
  GFO.Abs n     -> [| GFO.Abs  n |]
  GFO.App l m   -> [| GFO.App  l m |]
  GFO.Tpl m n   -> [| GFO.Tpl  m n |]
  GFO.Fst l     -> [| GFO.Fst  l |]
  GFO.Snd l     -> [| GFO.Snd  l |]
  GFO.LeT m n   -> [| GFO.LeT  m n |]
  GFO.Int i     -> [| GFO.Int  i |]
  GFO.Rat i     -> [| GFO.Rat  i |]
  GFO.Tag s m   -> [| GFO.Tag  s m |]
  GFO.Mem m     -> [| GFO.Mem  m |]
  GFO.Fix m     -> [| GFO.Fix  m |]
  GFO.Ary m n   -> [| GFO.Ary m n |]
  GFO.Len m     -> [| GFO.Len m |]
  GFO.Ind m n   -> [| GFO.Ind m n |]
  GFO.AryV m n  -> [| GFO.AryV m n |]
  GFO.LenV m    -> [| GFO.LenV m |]
  GFO.IndV m n  -> [| GFO.IndV m n |]
  GFO.Cmx m n   -> [| GFO.Cmx m n |]
  GFO.Non       -> [| GFO.Non |]
  GFO.Som m     -> [| GFO.Som m |]
  GFO.May l m n -> [| GFO.May l m n |]
  GFO.Mul m n   -> [| GFO.Mul m n |]
  GFO.Add m n   -> [| GFO.Add m n |]
  GFO.Sub m n   -> [| GFO.Sub m n |]
  GFO.Eql m n   -> [| GFO.Eql m n |]
  GFO.Ltd m n   -> [| GFO.Ltd m n |]
