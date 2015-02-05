{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.Lifting (cnvHOFOF,cnvFOHOF) where

import QFeldspar.MyPrelude
import QFeldspar.Conversion
import QFeldspar.Singleton
import QFeldspar.Expression.Utils.Common
import QFeldspar.Variable.Typed                             as VT
import QFeldspar.Environment.Typed                          as ET
import qualified QFeldspar.Expression.GADTFirstOrder        as FGFO
import qualified QFeldspar.Expression.GADTHigherOrder       as FGHO
import qualified QFeldspar.Type.GADT                        as TFG
import qualified QFeldspar.Environment.Plain                as EP
import qualified QFeldspar.Nat.ADT                          as NA
import qualified QFeldspar.Expression.Utils.GADTHigherOrder as FGHO
                 (sucAll,prdAll)
import QFeldspar.Variable.Conversion ()

instance (t ~ t' , r ~ r') =>
         Cnv (FGFO.Exp r t , Env TFG.Typ r) (FGHO.Exp r' t') where
  cnv (e , r) = pure (cnvFOHO r e)

instance (HasSin TFG.Typ t , t ~ t' , r ~ r') =>
         Cnv (FGHO.Exp r t , Env TFG.Typ r) (FGFO.Exp r' t') where
  cnv (e , r) = pure (cnvHOFO r e)

cnvFOHO :: Env tf r -> FGFO.Exp r t -> FGHO.Exp r t
cnvFOHO r e = cnvFOHO' ((ET.fmap FGHO.Var . cnvGEnvtoGVar) r) e

cnvFOHOF :: Env tf g -> FGFO.Exp (a ': g) b ->
         (FGHO.Exp g a -> FGHO.Exp g b)
cnvFOHOF r e = cnvFOHO'F ((ET.fmap FGHO.Var . cnvGEnvtoGVar) r) e

cnvHOFO :: (HasSin TFG.Typ t) =>
           Env TFG.Typ r -> FGHO.Exp r t -> FGFO.Exp r t
cnvHOFO r e = cnvHOFO' (genEnv r) r e

cnvHOFOF :: (HasSin TFG.Typ a, HasSin TFG.Typ b) =>
            Env TFG.Typ r' -> (FGHO.Exp r' a -> FGHO.Exp r' b) -> FGFO.Exp (a ': r') b
cnvHOFOF r f = cnvHOFO'F (genEnv r) r f

fmapVarEnv :: (forall a. Var r a -> Var r' a) -> VarEnv r -> VarEnv r'
fmapVarEnv _ EP.Emp                 = EP.Emp
fmapVarEnv f (EP.Ext (Exs1 v t) vs) = EP.Ext (Exs1 (f v) t) (fmapVarEnv f vs)

genEnv :: Env TFG.Typ r -> VarEnv r
genEnv ET.Emp        = EP.Emp
genEnv (ET.Ext t ts) = EP.Ext (Exs1 VT.Zro t) (fmapVarEnv VT.Suc (genEnv ts))

cnvGEnvtoGVar :: Env tf r -> Env (VT.Var r) r
cnvGEnvtoGVar ET.Emp        = ET.Emp
cnvGEnvtoGVar (ET.Ext _ xs) = ET.Ext VT.Zro
                              (ET.fmap VT.Suc (cnvGEnvtoGVar xs))

type VarEnv g = EP.Env (Exs1 (Var g) TFG.Typ)

incEP :: VarEnv g -> VarEnv (t ': g)
incEP []                = []
incEP ((Exs1 v t) : vs) = (Exs1 (Suc v) t) : incEP vs

cnvFOHO' :: forall r t.
        Env (FGHO.Exp r) r -> FGFO.Exp r t -> FGHO.Exp r t
cnvFOHO' r ee  = case ee of
  FGFO.Var v -> ET.get v r
  _          -> $(biGenOverloaded 'ee ''FGFO.Exp "FGHO" ['FGFO.Var]
   (\ tt -> if
    | matchQ tt [t| FGFO.Exp (t ': t) t |] -> [| cnvFOHO'F r |]
    | matchQ tt [t| FGFO.Exp t t |]        -> [| cnvFOHO'  r |]
    | otherwise                            -> [| id |]))

cnvFOHO'F :: Env (FGHO.Exp r) r -> FGFO.Exp (ta ': r) tb ->
         (FGHO.Exp r ta -> FGHO.Exp r tb)
cnvFOHO'F r f = (\ x ->
            FGHO.prdAll
            (cnvFOHO' (ET.fmap FGHO.sucAll
                      (Ext x r)) f))

cnvHOFO' :: forall r r' t. (HasSin TFG.Typ t) =>
           VarEnv r -> Env TFG.Typ r' -> FGHO.Exp r' t -> FGFO.Exp r t
cnvHOFO' r r' ee  = let t = sin :: TFG.Typ t in case ee of
  FGHO.Var v -> case frmRgt (EP.get (let n :: NA.Nat = (frmRgtZro $ cnv (v,()))
                                     in  ((EP.len r) `NA.sub` (ET.lenNat r')) `NA.add` n) r) of
                  Exs1 v' t' -> case frmRgt (eqlSin t t') of
                    Rfl      -> FGFO.Var v'
  FGHO.Tmp s -> case frmRgt (EP.get (let v' :: NA.Nat = NA.natStr s
                                     in  ((EP.len r) `NA.sub` (NA.Suc NA.Zro)) `NA.sub` v') r) of
                  Exs1 v' t' -> case frmRgt (eqlSin t t') of
                    Rfl      -> FGFO.Var v'
  _          -> $(biGenOverloadedW 'ee ''FGHO.Exp "FGFO" ['FGHO.Var,'FGHO.Tmp] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| FGHO.Exp t t -> FGHO.Exp t t |] -> [| cnvHOFO'F r r' |]
    | matchQ tt [t| FGHO.Exp t t |]        -> [| cnvHOFO' r r' |]
    | otherwise                            -> [| id |]))

cnvHOFO'F :: forall ta tb r r'.
            (HasSin TFG.Typ ta, HasSin TFG.Typ tb) =>
            VarEnv r -> Env TFG.Typ r' -> (FGHO.Exp r' ta -> FGHO.Exp r' tb) -> FGFO.Exp (ta ': r) tb
cnvHOFO'F r r' f =  let tag  = FGHO.Tmp (show (EP.len r))
                in  cnvHOFO' (EP.Ext (Exs1 Zro (sin :: TFG.Typ ta)) (incEP r)) r' (f tag)
