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
  cnv (ee  , r@(m , n)) = case ee of
    FAUD.Var x         -> FGTD.Var  <$> cnv (x , n)
    FAUD.Prm x es      -> FGTD.Prm  <$> pure (fmap (const Nothing) es) <*> cnv (x , m)
                                    <*> mapM (cnvWth r) es
    FAUD.App ef ea     -> FGTD.App  <$> pure Nothing  <*> cnvWth r ef <*> cnvWth r ea
    FAUD.Fst e         -> FGTD.Fst  <$> pure Nothing  <*> cnvWth r e
    FAUD.Snd e         -> FGTD.Snd  <$> pure Nothing  <*> cnvWth r e
    FAUD.Len e         -> FGTD.Len  <$> pure Nothing  <*> cnvWth r e
    FAUD.LenV e        -> FGTD.LenV <$> pure Nothing  <*> cnvWth r e
    FAUD.Eql el eb     -> FGTD.Eql  <$> pure Nothing  <*> cnvWth r el <*> cnvWth r eb
    FAUD.Ltd el eb     -> FGTD.Ltd  <$> pure Nothing  <*> cnvWth r el <*> cnvWth r eb
    FAUD.LeT el eb     -> FGTD.LeT  <$> pure Nothing  <*> cnvWth r el <*> cnvWth r eb
    FAUD.May em en es  -> FGTD.May  <$> pure Nothing  <*> cnvWth r em <*> cnvWth r en <*> cnvWth r es
    FAUD.Typ t  e      -> FGTD.Typ  <$> pure (Just t) <*> cnvWth r e
    _                  -> $(biGenOverloadedM 'ee ''FAUD.Exp "FGTD"
     ['FAUD.App,'FAUD.Fst,'FAUD.Snd,'FAUD.Len,'FAUD.LenV,'FAUD.Eql,'FAUD.Ltd,'FAUD.Prm,'FAUD.Var,
      'FAUD.LeT,'FAUD.May,'FAUD.Typ] (const [| cnvWth r |]))

instance (m ~ m' , n ~ n') =>
         Cnv (FAUD.Fun , (Nat m , Nat n)) (FGTD.Exp m' (NA.Suc n') (Maybe TFA.Typ)) where
  cnv (FAUD.Fun e , (m , n)) = cnv (e , (m , Suc n))
