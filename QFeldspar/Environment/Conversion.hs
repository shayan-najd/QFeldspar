module QFeldspar.Environment.Conversion () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Environment.Map    as EM
import qualified QFeldspar.Environment.Plain  as EP
import qualified QFeldspar.Environment.Typed  as ET
import qualified QFeldspar.Environment.Scoped as ES

import qualified QFeldspar.Type.ADT  as TFA
import qualified QFeldspar.Type.GADT as TFG

import QFeldspar.Conversion
import QFeldspar.Type.Conversion ()
import QFeldspar.Singleton
import qualified QFeldspar.Nat.GADT as NG

---------------------------------------------------------------------------------
-- Conversion from EM.Env
---------------------------------------------------------------------------------
instance Cnv (a , r) b =>
         Cnv (EM.Env x a , r) (EM.Env x b) where
  cnv (ee , r) =
    mapM (\ (x , y) -> do y' <- cnv (y , r)
                          return (x , y')) ee

---------------------------------------------------------------------------------
-- Conversion from EP.Env
---------------------------------------------------------------------------------
instance Cnv (a , r) b =>
         Cnv (EP.Env a , r) (EP.Env b)  where
  cnv (ee , r) = mapM (cnvWth r) ee

instance (n ~ n', a ~ a') => Cnv (EP.Env a , NG.Nat n) (ES.Env n' a')  where
  cnv ([]    , NG.Zro)   = return ES.Emp
  cnv (x : xs, NG.Suc n) = do xs' <- cnv (xs , n)
                              return (ES.Ext x xs')
  cnv _                  = fail "Conversion Error!"

instance Cnv (a , r) (ExsSin b) =>
         Cnv (EP.Env a , r) (ExsSin (ET.Env b)) where
  cnv (ee , r) = case ee of
    []    -> return (ExsSin ET.Emp)
    t : ts -> do ExsSin t' <- cnvWth r t
                 ExsSin r' <- cnvWth r ts
                 return (ExsSin (ET.Ext t' r'))

---------------------------------------------------------------------------------
-- Conversion from ES.Env
---------------------------------------------------------------------------------
instance Cnv (ES.Env n t , r ) (EP.Env t) where
  cnv (ee , r) = case ee of
    ES.Emp      -> pure []
    ES.Ext x xs -> (x :) <$> cnvWth r xs

instance (Cnv (a , r) b , n ~ n') =>
         Cnv (ES.Env n a , r) (ES.Env n' b) where
  cnv (ee , r) = mapM (cnvWth r) ee

---------------------------------------------------------------------------------
-- Conversion from ES.Env
---------------------------------------------------------------------------------
instance n ~ Len r =>
         Cnv (ET.Env TFG.Typ r , rr) (ES.Env n TFA.Typ) where
  cnv (ee , r) = case ee of
    ET.Emp      -> pure ES.Emp
    ET.Ext x xs -> ES.Ext <$> cnvWth r x <*> cnvWth r xs
