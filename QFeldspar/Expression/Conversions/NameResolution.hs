{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.NameResolution () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Expression.ADTUntypedNamed    as FAUN
import qualified QFeldspar.Expression.ADTUntypedDebruijn as FAUD

import qualified QFeldspar.Environment.Map                        as EM
import qualified QFeldspar.Environment.Plain                      as EP

import QFeldspar.Variable.Plain

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion    ()

instance Eq x =>
         Cnv (FAUN.Exp x , EP.Env x) FAUD.Exp where
  cnv (e , r) = cnv (e , zip r [Zro ..])

instance Eq x =>
         Cnv (FAUN.Exp x , EM.Env x Var) FAUD.Exp where
  cnv (ee , r) = let ?r = r in
   $(biRecAppMQ 'ee ''FAUN.Exp "FAUD")

instance Eq x =>
         Cnv ((x , FAUN.Exp x) , EM.Env x Var)
         FAUD.Fun where
  cnv ((x , e) , r) = fmap FAUD.Fun
                      (cnv (e , (x , Zro) : fmap (fmap Suc) r))

instance Cnv (Var, (EP.Env x', EM.Env Var x')) x' where
  cnv (v , r) = cnv (v , snd r)

instance (x ~ x') =>
         Cnv (FAUD.Exp , (EP.Env x , EM.Env Var x)) (FAUN.Exp x') where
  cnv (ee , r) = let ?r = r in
                 $(biRecAppMQ 'ee ''FAUD.Exp "FAUN")

instance (x ~ x') =>
         Cnv (FAUD.Fun , (EP.Env x , EM.Env Var x)) (x' , FAUN.Exp x')
         where
   cnv (FAUD.Fun e , r) = case r of
     (x : xs , r') -> do e' <- cnv (e ,
                                    (xs , (Zro , x) :
                                    fmap (\(v , n) -> (Suc v , n)) r'))
                         pure (x , e')
     _             -> fail "Bad Name Pool!"
