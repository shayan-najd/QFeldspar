{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.NameResolution () where

import QFeldspar.MyPrelude
import qualified QFeldspar.Expression.ADTUntypedNamed    as AUN
import qualified QFeldspar.Expression.ADTUntypedDebruijn as AUD
import qualified QFeldspar.Environment.Map                        as EM
import qualified QFeldspar.Environment.Plain                      as EP
import QFeldspar.Variable.Plain
import QFeldspar.Conversion
import QFeldspar.Variable.Conversion    ()

instance (Show x , Eq x) =>
         Cnv (AUN.Exp x , (EP.Env x , EP.Env x)) AUD.Exp where
  cnv (e , (s , g)) = cnv (e , (zip s [Zro ..] , zip g [Zro ..]))

instance (Show x , Eq x) =>
         Cnv (AUN.Exp x , (EM.Env x Var , EM.Env x Var)) AUD.Exp where
  cnv (ee , (s , g)) = case ee of
    AUN.Var x      -> AUD.Var <$> EM.get x g
    AUN.Prm x ns   -> AUD.Prm <$> EM.get x s <*> mapM (\m -> cnv (m , (s , g))) ns
    _               -> $(biGenOverloadedM 'ee ''AUN.Exp "AUD" ['AUN.Var,'AUN.Prm]
     (\ _tt -> [| \ m -> cnv (m , (s , g)) |]))


instance (Show x , Eq x) =>
         Cnv ((x , AUN.Exp x) ,(EM.Env x Var , EM.Env x Var))
         AUD.Fun where
  cnv ((x , m) , (s , g)) = fmap AUD.Fun
                            (cnv (m , (s , (x , Zro) : fmap (fmap Suc) g)))
{-
instance Cnv (Var, (EP.Env x', EM.Env Var x')) x' where
  cnv (v , r) = EM.get v (snd r)

instance (x ~ x') =>
         Cnv (AUD.Exp , (EP.Env x , EM.Env Var x)) (AUN.Exp x') where
  cnv (ee , r) = let ?r = r in case ee of
    AUD.Var v ->
    _          -> $(biRecAppMQW 'ee ''AUD.Exp "AUN" ['AUD.Var]
                                    (const id))

instance (x ~ x') =>
         Cnv (AUD.Fun , (EP.Env x , EM.Env Var x)) (x' , AUN.Exp x')
         where
   cnv (AUD.Fun e , r) = case r of
     (x : xs , r') -> do e' <- cnv (e ,
                                    (xs , (Zro , x) :
                                    fmap (\(v , n) -> (Suc v , n)) r'))
                         pure (x , e')
     _             -> fail "Bad Name Pool!"
-}
