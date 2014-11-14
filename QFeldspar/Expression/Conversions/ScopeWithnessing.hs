{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.ScopeWithnessing () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Expression.ADTUntypedDebruijn  as FAUD
import qualified QFeldspar.Expression.GADTTyped           as FGTD
import qualified QFeldspar.Type.ADT                       as TFA
import qualified QFeldspar.Nat.ADT                                 as NA
import QFeldspar.Nat.GADT

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()

instance n ~ n' => Cnv (FAUD.Exp , Nat n) (FGTD.Exp n' (Maybe TFA.Typ)) where
  cnv (ee  , n) = let ?r = n in case ee of
    FAUD.App ef ea    -> FGTD.App  <$> pure Nothing  <*@> ef <*@> ea
    FAUD.Fst e        -> FGTD.Fst  <$> pure Nothing  <*@> e
    FAUD.Snd e        -> FGTD.Snd  <$> pure Nothing  <*@> e
    FAUD.Len e        -> FGTD.Len  <$> pure Nothing  <*@> e
    FAUD.LenV e       -> FGTD.LenV <$> pure Nothing  <*@> e
    FAUD.Let el eb    -> FGTD.Let  <$> pure Nothing  <*@> el <*@> eb
    FAUD.May em en es -> FGTD.May  <$> pure Nothing  <*@> em <*@> en <*@> es
    FAUD.Typ t  e     -> FGTD.Typ  <$> pure (Just t) <*@> e
    _                 -> $(biRecAppMQW 'ee ''FAUD.Exp "FGTD"
     ['FAUD.App,'FAUD.Fst,'FAUD.Snd,'FAUD.Len,'FAUD.LenV
     ,'FAUD.Let,'FAUD.May,'FAUD.Typ] (const id))

instance n ~ n' => Cnv (FAUD.Fun , Nat n) (FGTD.Exp (NA.Suc n') (Maybe TFA.Typ)) where
  cnv (FAUD.Fun e , n) = cnv (e , Suc n)
