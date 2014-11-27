module QFeldspar.Variable.Conversion () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Variable.Plain     as VP
import qualified QFeldspar.Variable.Scoped    as VS
import qualified QFeldspar.Variable.Typed     as VT

import qualified QFeldspar.Environment.Map    as EM
import qualified QFeldspar.Environment.Plain  as EP
import qualified QFeldspar.Environment.Scoped as ES
import qualified QFeldspar.Environment.Typed  as ET

import qualified QFeldspar.Nat.GADT           as NG

import QFeldspar.Conversion
import QFeldspar.Environment.Conversion ()
import QFeldspar.Nat.Conversion         ()

import QFeldspar.Singleton

---------------------------------------------------------------------------------
-- Conversion from X
---------------------------------------------------------------------------------
instance Eq x =>
         Cnv (x , EM.Env x t) t        where
  cnv (x , r) = EM.get x r

---------------------------------------------------------------------------------
-- Conversion from VP
---------------------------------------------------------------------------------
instance Cnv (VP.Var, r) VP.Var where
  cnv = pure . fst

instance Cnv (VP.Var , NG.Nat n) (VS.Var n) where
  cnv (VP.Zro   , NG.Suc  _) = return VS.Zro
  cnv (VP.Suc n , NG.Suc n') = VS.Suc <$> cnv (n , n')
  cnv _                      = impossibleM

instance (HasSin NG.Nat (Len r) , HasSin tf t , EqlSin tf) =>
         Cnv (VP.Nat , ET.Env tf r) (VT.Var r t) where
  cnv (v , r) = do v' :: VS.Var (Len r) <- cnv (v , sin :: NG.Nat (Len r))
                   cnv (v' , r)

instance Cnv (VP.Var , EP.Env t) t where
  cnv (x , r) = EP.get x r
---------------------------------------------------------------------------------
-- Conversion from VS
---------------------------------------------------------------------------------
instance Cnv (VS.Var n , r) VP.Var where
  cnv (v , r) = let ?r = r in case v of
    VS.Zro   -> pure VP.Zro
    VS.Suc n -> VP.Suc <$@> n

instance n ~ n' =>
         Cnv (VS.Var n , r) (VS.Var n') where
  cnv = pure . fst

instance (n ~ Len r , r ~ r' , EqlSin tf , HasSin tf t) =>
         Cnv (VS.Var n, ET.Env tf r) (VT.Var r' t)  where
  cnv (VS.Zro   , ET.Ext x _ ) = do Rfl <- lift (eqlSin x (sin :: tf t))
                                    return VT.Zro
  cnv (VS.Suc n , ET.Ext _ xs) = VT.Suc <$> cnv (n , xs)
  cnv _                        = impossibleM

instance Cnv (VS.Var n   , ES.Env n  t) t      where
  cnv (x , r) = return (ES.get x r)

---------------------------------------------------------------------------------
-- Conversion from VT
---------------------------------------------------------------------------------
instance Cnv (VT.Var r t , rr) VP.Nat where
  cnv (v , r) = let ?r = r in
                do v' :: VS.Var (Len r) <- cnvImp v
                   cnvImp v'

instance (n ~ Len r) =>
         Cnv (VT.Var r t , rr) (VS.Var n) where
  cnv (v , r) = let ?r = r in case v of
    VT.Zro   -> pure VS.Zro
    VT.Suc n -> VS.Suc <$@> n

instance Cnv (VT.Var r t , ET.Env tf r) (tf t) where
  cnv (x , r) = return (ET.get x r)
