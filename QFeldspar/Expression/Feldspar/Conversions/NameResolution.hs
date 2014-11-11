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
         FAUD.Exp where
  cnv ((x , e) , r) = cnv (e , (x , Zro) : fmap (fmap Suc) r)

instance (x ~ x') =>
         Cnv (FAUD.Exp , (EP.Env x , EM.Env Var x)) (FAUN.Exp x') where
  cnv (eaup , r) = let ?r = r in case eaup of
    FAUD.ConI i        -> pure (FAUN.ConI i)
    FAUD.ConB b        -> pure (FAUN.ConB b)
    FAUD.ConF f        -> pure (FAUN.ConF f)
    FAUD.Var  x        -> let ?r = snd r in FAUN.Var <$@> x
    FAUD.Abs  eb       -> FAUN.Abs <$@> eb
    FAUD.App  ef ea    -> FAUN.App <$@> ef <*@> ea
    FAUD.Cnd  ec et ef -> FAUN.Cnd <$@> ec <*@> et <*@> ef
    FAUD.Whl  ec eb ei -> FAUN.Whl <$@> ec <*@> eb <*@> ei
    FAUD.Tpl  ef es    -> FAUN.Tpl <$@> ef <*@> es
    FAUD.Fst  e        -> FAUN.Fst <$@> e
    FAUD.Snd  e        -> FAUN.Snd <$@> e
    FAUD.Ary  el ef    -> FAUN.Ary <$@> el <*@> ef
    FAUD.Len  e        -> FAUN.Len <$@> e
    FAUD.Ind  ea ei    -> FAUN.Ind <$@> ea <*@> ei
    FAUD.AryV  el ef   -> FAUN.AryV <$@> el <*@> ef
    FAUD.LenV  e       -> FAUN.LenV <$@> e
    FAUD.IndV  ea ei   -> FAUN.IndV <$@> ea <*@> ei
    FAUD.Let  el eb    -> FAUN.Let <$@> el <*@> eb
    FAUD.Cmx  er ei    -> FAUN.Cmx <$@> er <*@> ei
    FAUD.Non           -> pure FAUN.Non
    FAUD.Som  e        -> FAUN.Som <$@> e
    FAUD.May  em en es -> FAUN.May <$@> em <*@> en <*@> es
    FAUD.Typ  t  e     -> FAUN.Typ <$> pure t <*@> e
    FAUD.Mul  er ei    -> FAUN.Mul <$@> er <*@> ei

instance (x ~ x') =>
         Cnv (FAUD.Exp , (EP.Env x , EM.Env Var x)) (x' , FAUN.Exp x') where
   cnv (e , r) = case r of
     (x : xs , r') -> do e' <- cnv (e ,
                                    (xs , (Zro , x) :
                                        fmap (\(v , n) -> (Suc v , n)) r'))
                         pure (x , e')
     _             -> fail "Bad Name Pool!"
