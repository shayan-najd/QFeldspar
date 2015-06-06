{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.Lifting (cnvHOFOF,cnvFOHO,cnvFOHOF) where

import QFeldspar.MyPrelude
import QFeldspar.Conversion
import QFeldspar.Singleton
import QFeldspar.Expression.Utils.Common
import QFeldspar.Variable.Typed                             as VT
import QFeldspar.Environment.Typed                          as ET
import qualified QFeldspar.Expression.GADTFirstOrder        as GFO
import qualified QFeldspar.Expression.GADTHigherOrder       as GHO
import qualified QFeldspar.Type.GADT                        as TG
import qualified QFeldspar.Environment.Plain                as EP
import qualified QFeldspar.Nat.ADT                          as NA
import QFeldspar.Variable.Conversion ()

instance (a ~ a' , s ~ s') =>
         Cnv (GFO.Exp s '[] a , r) (GHO.Exp s' a') where
  cnv (e , _) = pure (cnvFOHO e)

instance (TG.Type a , a ~ a' , s ~ s') =>
         Cnv (GHO.Exp s a , Env TG.Typ s) (GFO.Exp s' '[] a') where
  cnv (e , s) = pure (cnvHOFO s e)

cnvFOHO :: GFO.Exp s '[] a -> GHO.Exp s a
cnvFOHO e = cnvFOHO' Emp e

cnvFOHOF :: GFO.Exp s '[a] b ->
            (GHO.Exp s a -> GHO.Exp s b)
cnvFOHOF e = cnvFOHO'F Emp e

cnvHOFO :: TG.Type a =>
           Env TG.Typ s -> GHO.Exp s a -> GFO.Exp s '[] a
cnvHOFO s e = cnvHOFO' [] s e

cnvHOFOF :: (TG.Type a, TG.Type b) =>
            Env TG.Typ s -> (GHO.Exp s a -> GHO.Exp s b) -> GFO.Exp s '[a] b
cnvHOFOF s f = cnvHOFO'F [] s f

type VarEnv g = EP.Env (Exs1 (Var g) TG.Typ)

incEP :: VarEnv g -> VarEnv (a ': g)
incEP []                = []
incEP ((Exs1 v t) : vs) = (Exs1 (Suc v) t) : incEP vs

cnvFOHO' :: forall s g a.
            Env (GHO.Exp s) g -> GFO.Exp s g a -> GHO.Exp s a
cnvFOHO' g ee  = case ee of
  GFO.Var x -> ET.get x g
  _          -> $(biGenOverloaded 'ee ''GFO.Exp "GHO" ['GFO.Var]
   (\ tt -> if
    | matchQ tt [t| Env (GFO.Exp a a)  a |] -> [| ET.fmap (cnvFOHO' g) |]
    | matchQ tt [t| GFO.Exp a (a ': a) a |] -> [| cnvFOHO'F g |]
    | matchQ tt [t| GFO.Exp a a a |]        -> [| cnvFOHO'  g |]
    | otherwise                              -> [| id |]))

cnvFOHO'F :: Env (GHO.Exp s) g -> GFO.Exp s (a ': g) b ->
             (GHO.Exp s a -> GHO.Exp s b)
cnvFOHO'F g f = (\ x -> cnvFOHO' (Ext x g) f)

cnvHOFO' :: forall s g a. TG.Type a =>
            VarEnv g -> Env TG.Typ s -> GHO.Exp s a -> GFO.Exp s g a
cnvHOFO' g s ee  = let t = sin :: TG.Typ a in case ee of
  GHO.Tmp x -> case frmRgt (EP.get (let v' :: NA.Nat = NA.natStr x
                                     in  ((EP.len g) `NA.sub` (NA.Suc NA.Zro)) `NA.sub` v') g) of
                  Exs1 v' t' -> case frmRgt (eqlSin t t') of
                    Rfl      -> GFO.Var v'
  GHO.Prm v es -> GFO.Prm v (TG.mapC (cnvHOFO' g s) es)
  _          -> $(biGenOverloadedW 'ee ''GHO.Exp "GFO" ['GHO.Prm,'GHO.Tmp] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| GHO.Exp a a -> GHO.Exp a a |] -> [| cnvHOFO'F g s |]
    | matchQ tt [t| GHO.Exp a a |]                 -> [| cnvHOFO'  g s |]
    | otherwise                                     -> [| id |]))

cnvHOFO'F :: forall a b s g.
            (TG.Type a, TG.Type b) =>
            VarEnv g -> Env TG.Typ s -> (GHO.Exp s a -> GHO.Exp s b) -> GFO.Exp s (a ': g) b
cnvHOFO'F g s f =  let tag  = GHO.Tmp (show (EP.len g))
                   in  cnvHOFO' (EP.Ext (Exs1 Zro (sin :: TG.Typ a)) (incEP g)) s (f tag)
