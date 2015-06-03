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

instance (Show x , Eq x) =>
         Cnv (FAUN.Exp x , (EP.Env x , EP.Env x)) FAUD.Exp where
  cnv (e , (s , g)) = cnv (e , (zip s [Zro ..] , zip g [Zro ..]))

instance (Show x , Eq x) =>
         Cnv (FAUN.Exp x , (EM.Env x Var , EM.Env x Var)) FAUD.Exp where
  cnv (ee , (s , g)) = case ee of
    FAUN.Var x      -> FAUD.Var <$> EM.get x g
    FAUN.Prm x ns   -> FAUD.Prm <$> EM.get x s <*> mapM (\m -> cnv (m , (s , g))) ns
    _               -> $(biGenOverloadedM 'ee ''FAUN.Exp "FAUD" ['FAUN.Var,'FAUN.Prm]
     (\ _tt -> [| \ m -> cnv (m , (s , g)) |]))


instance (Show x , Eq x) =>
         Cnv ((x , FAUN.Exp x) ,(EM.Env x Var , EM.Env x Var))
         FAUD.Fun where
  cnv ((x , m) , (s , g)) = fmap FAUD.Fun
                            (cnv (m , (s , (x , Zro) : fmap (fmap Suc) g)))
{-
instance Cnv (Var, (EP.Env x', EM.Env Var x')) x' where
  cnv (v , r) = EM.get v (snd r)

instance (x ~ x') =>
         Cnv (FAUD.Exp , (EP.Env x , EM.Env Var x)) (FAUN.Exp x') where
  cnv (ee , r) = let ?r = r in case ee of
    FAUD.Var v ->
    _          -> $(biRecAppMQW 'ee ''FAUD.Exp "FAUN" ['FAUD.Var]
                                    (const id))

instance (x ~ x') =>
         Cnv (FAUD.Fun , (EP.Env x , EM.Env Var x)) (x' , FAUN.Exp x')
         where
   cnv (FAUD.Fun e , r) = case r of
     (x : xs , r') -> do e' <- cnv (e ,
                                    (xs , (Zro , x) :
                                    fmap (\(v , n) -> (Suc v , n)) r'))
                         pure (x , e')
     _             -> fail "Bad Name Pool!"
-}
