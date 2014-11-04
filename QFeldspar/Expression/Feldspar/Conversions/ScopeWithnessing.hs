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
    FAUD.Whl ec eb ei -> FGTD.Whl  <$> cnvf ec <*> cnvf eb <*@> ei
    FAUD.Tpl ef es    -> FGTD.Tpl  <$@> ef <*@> es
    FAUD.Fst e        -> FGTD.Fst  <$> pure Nothing <*@> e
    FAUD.Snd e        -> FGTD.Snd  <$> pure Nothing <*@> e
    FAUD.Ary el ef    -> FGTD.Ary  <$@> el <*> cnvf ef
    FAUD.Len e        -> FGTD.Len  <$> pure Nothing <*@> e
    FAUD.Ind ea ei    -> FGTD.Ind  <$@> ea <*@> ei
    FAUD.Let el eb    -> FGTD.Let  <$> pure Nothing <*@> el <*> cnvf eb
    FAUD.Cmx er ei    -> FGTD.Cmx  <$@> er <*@> ei
    FAUD.Non          -> pure FGTD.Non
    FAUD.Som e        -> FGTD.Som  <$@> e
    FAUD.May em en es -> FGTD.May  <$> pure Nothing <*@> em <*@> en <*> cnvf es
    where
      cnvf e = cnv (e , Suc n)

instance Cnv (FGTD.Exp n t , r) (FAUD.Exp) where
  cnv (eaum  , r) = let ?r = r in case eaum of
    FGTD.ConI i       -> pure (FAUD.ConI i)
    FGTD.ConB b       -> pure (FAUD.ConB b)
    FGTD.ConF f       -> pure (FAUD.ConF f)
    FGTD.Var v        -> FAUD.Var  <$@> v
    FGTD.Abs eb       -> FAUD.Abs  <$@> eb
    FGTD.App _ ef ea  -> FAUD.App  <$@> ef <*@> ea
    FGTD.Cnd ec et ef -> FAUD.Cnd  <$@> ec <*@> et <*@> ef
    FGTD.Whl ec eb ei -> FAUD.Whl  <$@> ec <*@> eb <*@> ei
    FGTD.Tpl ef es    -> FAUD.Tpl  <$@> ef <*@> es
    FGTD.Fst _ e      -> FAUD.Fst  <$@> e
    FGTD.Snd _ e      -> FAUD.Snd  <$@> e
    FGTD.Ary el ef    -> FAUD.Ary  <$@> el <*@> ef
    FGTD.Len _ e      -> FAUD.Len  <$@> e
    FGTD.Ind ea ei    -> FAUD.Ind  <$@> ea <*@> ei
    FGTD.Let _ el eb  -> FAUD.Let  <$@> el <*@> eb
    FGTD.Cmx er ei    -> FAUD.Cmx  <$@> er <*@> ei
    FGTD.Non          -> pure FAUD.Non
    FGTD.Som e        -> FAUD.Som  <$@> e
    FGTD.May _ em en es -> FAUD.May  <$@> em <*@> en <*@> es