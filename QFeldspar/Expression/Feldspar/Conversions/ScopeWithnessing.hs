module QFeldspar.Expression.Feldspar.Conversions.ScopeWithnessing () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Expression.Feldspar.ADTUntypedDebruijn  as FAUD
import qualified QFeldspar.Expression.Feldspar.GADTTyped           as FGTD
import qualified QFeldspar.Type.Feldspar.ADT                       as TFA

import QFeldspar.Nat.GADT

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()

instance n ~ n' => Cnv (FAUD.Exp , Nat n) (FGTD.Exp n' (Maybe TFA.Typ)) where
  cnv (eaum  , n) = let ?r = n in case eaum of
    FAUD.ConI i       -> pure (FGTD.ConI i)
    FAUD.ConB b       -> pure (FGTD.ConB b)
    FAUD.ConF f       -> pure (FGTD.ConF f)
    FAUD.Var v        -> FGTD.Var  <$@> v
    FAUD.Abs eb       -> FGTD.Abs  <$> cnvf eb
    FAUD.App ef ea    -> FGTD.App  <$> pure Nothing <*@> ef <*@> ea
    FAUD.Cnd ec et ef -> FGTD.Cnd  <$@> ec <*@> et <*@> ef
    FAUD.Whl ec eb ei -> FGTD.Whl  <$@> ec <*@> eb <*@> ei
    FAUD.Tpl ef es    -> FGTD.Tpl  <$@> ef <*@> es
    FAUD.Fst e        -> FGTD.Fst  <$> pure Nothing <*@> e
    FAUD.Snd e        -> FGTD.Snd  <$> pure Nothing <*@> e
    FAUD.Ary el ef    -> FGTD.Ary  <$@> el <*@> ef
    FAUD.Len e        -> FGTD.Len  <$> pure Nothing <*@> e
    FAUD.Ind ea ei    -> FGTD.Ind  <$@> ea <*@> ei
    FAUD.AryV el ef   -> FGTD.AryV <$@> el <*@> ef
    FAUD.LenV e       -> FGTD.LenV <$> pure Nothing <*@> e
    FAUD.IndV ea ei   -> FGTD.IndV <$@> ea <*@> ei
    FAUD.Let el eb    -> FGTD.Let  <$> pure Nothing <*@> el <*> cnvf eb
    FAUD.Cmx er ei    -> FGTD.Cmx  <$@> er <*@> ei
    FAUD.Non          -> pure FGTD.Non
    FAUD.Som e        -> FGTD.Som  <$@> e
    FAUD.May em en es -> FGTD.May  <$> pure Nothing <*@> em <*@> en <*@> es
    FAUD.Typ t  e     -> FGTD.Typ  <$> pure (Just t) <*@> e
    FAUD.Mul er ei    -> FGTD.Mul  <$@> er <*@> ei
    where
      cnvf e = cnv (e , Suc n)
