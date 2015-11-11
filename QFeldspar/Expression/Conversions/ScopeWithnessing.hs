module QFeldspar.Expression.Conversions.ScopeWithnessing () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Expression.ADTUntypedDebruijn as AUD
import qualified QFeldspar.Expression.GADTTyped as GTD
import qualified QFeldspar.Type.ADT as TA
import qualified QFeldspar.Nat.ADT as NA

import QFeldspar.Nat.GADT

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()

instance (m ~ m' , n ~ n') =>
         Cnv (AUD.Exp , (Nat m , Nat n))
             (GTD.Exp m' n' (Maybe TA.Typ)) where
  cnv (ee  , r@(m , n)) = case ee of
    AUD.Var x         -> GTD.Var  <$> cnv (x , n)
    AUD.Prm x es      -> GTD.Prm  <$> pure (fmap (const Nothing) es) <*> cnv (x , m)
                                    <*> mapM (cnvWth r) es
    AUD.App ef ea     -> GTD.App  <$> pure Nothing  <*> cnvWth r ef <*> cnvWth r ea
    AUD.Fst e         -> GTD.Fst  <$> pure Nothing  <*> cnvWth r e
    AUD.Snd e         -> GTD.Snd  <$> pure Nothing  <*> cnvWth r e
    AUD.Len e         -> GTD.Len  <$> pure Nothing  <*> cnvWth r e
    AUD.LenV e        -> GTD.LenV <$> pure Nothing  <*> cnvWth r e
    AUD.Eql el eb     -> GTD.Eql  <$> pure Nothing  <*> cnvWth r el <*> cnvWth r eb
    AUD.Ltd el eb     -> GTD.Ltd  <$> pure Nothing  <*> cnvWth r el <*> cnvWth r eb
    AUD.LeT el eb     -> GTD.LeT  <$> pure Nothing  <*> cnvWth r el <*> cnvWth r eb
    AUD.May em en es  -> GTD.May  <$> pure Nothing  <*> cnvWth r em <*> cnvWth r en <*> cnvWth r es
    AUD.Typ t  e      -> GTD.Typ  <$> pure (Just t) <*> cnvWth r e
    _                  -> $(biGenOverloadedM 'ee ''AUD.Exp "GTD"
     ['AUD.App,'AUD.Fst,'AUD.Snd,'AUD.Len,'AUD.LenV,'AUD.Eql,'AUD.Ltd,'AUD.Prm,'AUD.Var,
      'AUD.LeT,'AUD.May,'AUD.Typ] (const [| cnvWth r |]))

instance (m ~ m' , n ~ n') =>
         Cnv (AUD.Fun , (Nat m , Nat n)) (GTD.Exp m' ('NA.Suc n')
                                          (Maybe TA.Typ)) where
  cnv (AUD.Fun e , (m , n)) = cnv (e , (m , Suc n))
