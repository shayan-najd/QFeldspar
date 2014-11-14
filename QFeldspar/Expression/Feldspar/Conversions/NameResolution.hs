module QFeldspar.Expression.Feldspar.Conversions.NameResolution () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Expression.Feldspar.ADTUntypedNamed    as FAUN
import qualified QFeldspar.Expression.Feldspar.ADTUntypedDebruijn as FAUD

import qualified QFeldspar.Environment.Map                        as EM
import qualified QFeldspar.Environment.Plain                      as EP

import QFeldspar.Variable.Plain

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion    ()

instance Eq x =>
         Cnv (FAUN.Exp x , EP.Env x) FAUD.Exp where
  cnv (e , r) = cnv (e , zip r [Zro ..])

instance Eq x =>
         Cnv (FAUN.Exp x , EM.Env x Var) FAUD.Exp where
  cnv (ee , r) = let ?r = r in case ee of
    FAUN.ConI i              -> pure (FAUD.ConI i)
    FAUN.ConB b              -> pure (FAUD.ConB b)
    FAUN.ConF f              -> pure (FAUD.ConF f)
    FAUN.Var  x              -> FAUD.Var <$@> x
    FAUN.Abs  xeb            -> FAUD.Abs <$@> xeb
    FAUN.App  ef ea          -> FAUD.App <$@> ef  <*@> ea
    FAUN.Cnd  ec et ef       -> FAUD.Cnd <$@> ec  <*@> et  <*@> ef
    FAUN.Whl  xec xeb ei     -> FAUD.Whl <$@> xec <*@> xeb <*@> ei
    FAUN.Tpl  ef es          -> FAUD.Tpl <$@> ef  <*@> es
    FAUN.Fst  e              -> FAUD.Fst <$@> e
    FAUN.Snd  e              -> FAUD.Snd <$@> e
    FAUN.Ary  el xef         -> FAUD.Ary <$@> el <*@> xef
    FAUN.Len  e              -> FAUD.Len <$@> e
    FAUN.Ind  ea ei          -> FAUD.Ind <$@> ea <*@> ei
    FAUN.AryV  el xef        -> FAUD.AryV <$@> el <*@> xef
    FAUN.LenV  e             -> FAUD.LenV <$@> e
    FAUN.IndV  ea ei         -> FAUD.IndV <$@> ea  <*@> ei
    FAUN.Let  el xeb         -> FAUD.Let <$@> el  <*@> xeb
    FAUN.Cmx  er ei          -> FAUD.Cmx <$@> er  <*@> ei
    FAUN.Non                 -> pure FAUD.Non
    FAUN.Som  e              -> FAUD.Som <$@> e
    FAUN.May  em en xes      -> FAUD.May <$@> em <*@> en <*@> xes
    FAUN.Typ  t  e           -> FAUD.Typ <$> pure t <*@> e
    FAUN.Mul  er ei          -> FAUD.Mul <$@> er  <*@> ei

instance Eq x =>
         Cnv ((x , FAUN.Exp x) , EM.Env x Var)
         FAUD.Fun where
 cnv ((x , e) , r) = fmap FAUD.Fun
                     (cnv (e , (x , Zro) : fmap (fmap Suc) r))
