{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.Lifting (cnvHOFOF,cnvFOHO,cnvFOHOF) where

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
import QFeldspar.Variable.Conversion ()

instance (a ~ a' , s ~ s') =>
         Cnv (FGFO.Exp s '[] a , r) (FGHO.Exp s' a') where
  cnv (e , _) = pure (cnvFOHO e)

instance (HasSin TFG.Typ a , a ~ a' , s ~ s') =>
         Cnv (FGHO.Exp s a , Env TFG.Typ s) (FGFO.Exp s' '[] a') where
  cnv (e , s) = pure (cnvHOFO s e)

cnvFOHO :: FGFO.Exp s '[] a -> FGHO.Exp s a
cnvFOHO e = cnvFOHO' Emp e

cnvFOHOF :: FGFO.Exp s '[a] b ->
            (FGHO.Exp s a -> FGHO.Exp s b)
cnvFOHOF e = cnvFOHO'F Emp e

cnvHOFO :: HasSin TFG.Typ a =>
           Env TFG.Typ s -> FGHO.Exp s a -> FGFO.Exp s '[] a
cnvHOFO s e = cnvHOFO' [] s e

cnvHOFOF :: (HasSin TFG.Typ a, HasSin TFG.Typ b) =>
            Env TFG.Typ s -> (FGHO.Exp s a -> FGHO.Exp s b) -> FGFO.Exp s '[a] b
cnvHOFOF s f = cnvHOFO'F [] s f

type VarEnv g = EP.Env (Exs1 (Var g) TFG.Typ)

incEP :: VarEnv g -> VarEnv (a ': g)
incEP []                = []
incEP ((Exs1 v t) : vs) = (Exs1 (Suc v) t) : incEP vs

cnvFOHO' :: forall s g a.
            Env (FGHO.Exp s) g -> FGFO.Exp s g a -> FGHO.Exp s a
cnvFOHO' g ee  = case ee of
  FGFO.Var x -> ET.get x g
  _          -> $(biGenOverloaded 'ee ''FGFO.Exp "FGHO" ['FGFO.Var]
   (\ tt -> if
    | matchQ tt [t| Env (FGFO.Exp a a)  a |] -> [| ET.fmap (cnvFOHO' g) |]
    | matchQ tt [t| FGFO.Exp a (a ': a) a |] -> [| cnvFOHO'F g |]
    | matchQ tt [t| FGFO.Exp a a a |]        -> [| cnvFOHO'  g |]
    | otherwise                              -> [| id |]))

cnvFOHO'F :: Env (FGHO.Exp s) g -> FGFO.Exp s (a ': g) b ->
             (FGHO.Exp s a -> FGHO.Exp s b)
cnvFOHO'F g f = (\ x -> cnvFOHO' (Ext x g) f)

cnvHOFO' :: forall s g a. HasSin TFG.Typ a =>
            VarEnv g -> Env TFG.Typ s -> FGHO.Exp s a -> FGFO.Exp s g a
cnvHOFO' g s ee  = let t = sin :: TFG.Typ a in case ee of
  FGHO.Tmp x -> case frmRgt (EP.get (let v' :: NA.Nat = NA.natStr x
                                     in  ((EP.len g) `NA.sub` (NA.Suc NA.Zro)) `NA.sub` v') g) of
                  Exs1 v' t' -> case frmRgt (eqlSin t t') of
                    Rfl      -> FGFO.Var v'
  FGHO.Prm v es -> FGFO.Prm v (TFG.mapC (cnvHOFO' g s) es)
  _          -> $(biGenOverloadedW 'ee ''FGHO.Exp "FGFO" ['FGHO.Prm,'FGHO.Tmp] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| FGHO.Exp a a -> FGHO.Exp a a |] -> [| cnvHOFO'F g s |]
    | matchQ tt [t| FGHO.Exp a a |]                 -> [| cnvHOFO'  g s |]
    | otherwise                                     -> [| id |]))

cnvHOFO'F :: forall a b s g.
            (HasSin TFG.Typ a, HasSin TFG.Typ b) =>
            VarEnv g -> Env TFG.Typ s -> (FGHO.Exp s a -> FGHO.Exp s b) -> FGFO.Exp s (a ': g) b
cnvHOFO'F g s f =  let tag  = FGHO.Tmp (show (EP.len g))
                   in  cnvHOFO' (EP.Ext (Exs1 Zro (sin :: TFG.Typ a)) (incEP g)) s (f tag)
