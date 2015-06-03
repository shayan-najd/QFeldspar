{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.ScopeWithnessing () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Expression.ADTUntypedDebruijn as FAUD
import qualified QFeldspar.Expression.GADTTyped as FGTD
import qualified QFeldspar.Type.ADT as TFA
import qualified QFeldspar.Nat.ADT as NA

import QFeldspar.Nat.GADT

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()

instance (m ~ m' , n ~ n') =>
         Cnv (FAUD.Exp , (Nat m , Nat n))
             (FGTD.Exp m' n' (Maybe TFA.Typ)) where
  cnv (ee  , r@(m , n)) = let ?r = r in case ee of
    FAUD.Var x         -> FGTD.Var  <$> cnv (x , n)
    FAUD.Prm x es      -> FGTD.Prm  <$> pure Nothing <*> cnv (x , m)
                                    <*> mapM (\ e -> cnv (e , r)) es
    FAUD.App ef ea     -> FGTD.App  <$> pure Nothing  <*@> ef <*@> ea
    FAUD.Fst e         -> FGTD.Fst  <$> pure Nothing  <*@> e
    FAUD.Snd e         -> FGTD.Snd  <$> pure Nothing  <*@> e
    FAUD.Len e         -> FGTD.Len  <$> pure Nothing  <*@> e
    FAUD.LenV e        -> FGTD.LenV <$> pure Nothing  <*@> e
    FAUD.Eql el eb     -> FGTD.Eql  <$> pure Nothing  <*@> el <*@> eb
    FAUD.Ltd el eb     -> FGTD.Ltd  <$> pure Nothing  <*@> el <*@> eb
    FAUD.Let el eb     -> FGTD.Let  <$> pure Nothing  <*@> el <*@> eb
    FAUD.May em en es  -> FGTD.May  <$> pure Nothing  <*@> em <*@> en <*@> es
    FAUD.Typ t  e      -> FGTD.Typ  <$> pure (Just t) <*@> e
    _                  -> $(biGenOverloadedM 'ee ''FAUD.Exp "FGTD"
     ['FAUD.App,'FAUD.Fst,'FAUD.Snd,'FAUD.Len,'FAUD.LenV,'FAUD.Eql,'FAUD.Ltd,'FAUD.Prm,'FAUD.Var,
      'FAUD.Let,'FAUD.May,'FAUD.Typ] (const [| cnvImp |]))

instance (m ~ m' , n ~ n') =>
         Cnv (FAUD.Fun , (Nat m , Nat n)) (FGTD.Exp m' (NA.Suc n') (Maybe TFA.Typ)) where
  cnv (FAUD.Fun e , (m , n)) = cnv (e , (m , Suc n) )
