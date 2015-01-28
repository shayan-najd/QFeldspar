module QFeldspar.Normalisation.GADTFirstOrder () where

import QFeldspar.MyPrelude

import QFeldspar.Expression.GADTFirstOrder
import QFeldspar.Expression.Utils.GADTFirstOrder
    (sucAll,sbs,replaceOne,isFresh)

import QFeldspar.Variable.Typed
import QFeldspar.Normalisation
import QFeldspar.Singleton
import qualified QFeldspar.Type.GADT as TFG

isVal :: Exp n t -> Bool
isVal ee = case ee of
    ConI _        -> True
    ConB _        -> True
    ConF _        -> True
    Var  _        -> True
    Abs  _        -> True
    App  _  _     -> False
    Cnd  _  _  _  -> False
    Whl  _  _  _  -> False
    Tpl  ef es    -> isVal ef && isVal es
    Fst  _        -> False
    Snd  _        -> False
    Ary  el  _    -> isVal el
    Len  _        -> False
    Ind  _  _     -> False
    AryV el  _    -> isVal el
    LenV  _       -> False
    IndV  _  _    -> False
    Let  _  _     -> False
    Cmx  _  _     -> True
    Non           -> True
    Som  e        -> isVal e
    May  _ _  _   -> False
    Mul _ _       -> False
    Int _         -> True -- shouldn't matter

val :: Exp n t -> (Bool,Exp n t)
val ee = (isVal ee , ee)

pattern V  v <- (val -> (True  , v))
pattern NV v <- (val -> (False , v))

instance HasSin TFG.Typ t => NrmOne (Exp n t) where
  nrmOne ee = let t = sin :: TFG.Typ t in case ee of
    ConI i                       -> pure (ConI i)
    ConB b                       -> pure (ConB b)
    ConF f                       -> pure (ConF f)
    Var x                        -> pure (Var  x)
    Abs eb                       -> case TFG.getPrfHasSinArr t of
      (PrfHasSin , PrfHasSin)    -> Abs  <$@> eb
    App ef             (NV ea)   -> chg (Let ea (App (sucAll ef) (Var Zro)))
    App (Abs eb)       (V  ea)   -> chg (sbs ea eb)
    App (Cnd (V ec) et ef)(V ea) -> chg (Cnd ec (App et ea) (App ef ea))
    App (Let (NV el) eb) (V ea)  -> chg (Let el (App eb (sucAll ea)))
    App ef             ea        -> App  <$@> ef <*@> ea

    Cnd (NV ec)      et ef       -> chg (Let ec (Cnd (Var Zro) (sucAll et) (sucAll ef)))
    Cnd (ConB True)  et _        -> chg et
    Cnd (ConB False) _  ef       -> chg ef
    Cnd ec           et ef       -> Cnd  <$@> ec <*@> et <*@> ef

    Whl (NV ec) eb      ei       -> chg (Let ec (Whl (Var Zro) (sucAll eb) (sucAll ei)))
    Whl (V  ec) (NV eb) ei       -> chg (Let eb (Whl (sucAll ec) (Var Zro)  (sucAll ei)))
    Whl (V  ec) (V  eb) (NV ei)  -> chg (Let ei (Whl (sucAll ec) (sucAll eb) (Var Zro)))
    Whl ec eb ei                 -> Whl  <$@> ec <*@> eb <*@> ei

    Tpl (NV ef) es               -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin)    -> chg (Let ef (Tpl (Var Zro) (sucAll es)))
    Tpl (V ef)  (NV es)          -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin)    -> chg (Let es (Tpl (sucAll ef) (Var Zro)))
    Tpl ef      es               -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin)    -> Tpl  <$@> ef <*@> es

    Fst (NV e)                   -> chg (Let e (Fst (Var Zro)))
    Fst (Tpl (V ef) (V _))       -> chg  ef
    Fst e                        -> Fst  <$@> e

    Snd (NV e)                   -> chg (Let e (Snd (Var Zro)))
    Snd (Tpl (V _)  (V es))      -> chg  es
    Snd e                        -> Snd  <$@> e

    Ary (NV el) ef               -> chg (Let el (Ary (Var Zro) (sucAll ef)))
    Ary (V  el) (NV ef)          -> case TFG.getPrfHasSinAry t of
      PrfHasSin                  -> chg (Let ef (Ary (sucAll el) (Var Zro)))
    Ary el      ef               -> case TFG.getPrfHasSinAry t of
      PrfHasSin                  -> Ary  <$@> el <*@> ef

    Len (NV ea)                  -> chg (Let ea (Len (Var Zro)))
    Len (Ary (V el) _)           -> chg  el
    Len e                        -> Len  <$@> e

    Ind (NV ea)        ei        -> chg (Let ea (Ind (Var Zro) (sucAll ei)))
    Ind (V ea)         (NV ei)   -> chg (Let ei (Ind (sucAll ea) (Var Zro)))
    Ind (Ary (V _) ef) (V ei)    -> chg (App ef ei)
    Ind ea             ei        -> Ind  <$@> ea <*@> ei

    AryV (NV el) ef              -> chg (Let el (AryV (Var Zro) (sucAll ef)))
    AryV (V  el) (NV ef)         -> case TFG.getPrfHasSinVec t of
      PrfHasSin                  -> chg (Let ef (AryV (sucAll el) (Var Zro)))
    AryV el      ef              -> case TFG.getPrfHasSinVec t of
      PrfHasSin                  -> AryV  <$@> el <*@> ef

    LenV (NV ea)                  -> chg (Let ea (LenV (Var Zro)))
    LenV (AryV (V el) _)          -> chg  el
    LenV e                        -> LenV  <$@> e

    IndV (NV ea)        ei        -> chg (Let ea (IndV (Var Zro)  (sucAll ei)))
    IndV (V ea)         (NV ei)   -> chg (Let ei (IndV (sucAll ea) (Var Zro)))
    IndV (AryV (V _) ef) (V ei)   -> chg (App ef ei)
    IndV ea             ei        -> IndV  <$@> ea <*@> ei

    Cmx (NV er) ei               -> chg (Let er (Cmx  (Var Zro)  (sucAll ei)))
    Cmx (V er)  (NV ei)          -> chg (Let ei (Cmx  (sucAll er) (Var Zro) ))
    Cmx er ei                    -> Cmx  <$@> er <*@> ei

    Let (Let (NV el') eb')  eb   -> chg (Let el' (Let eb' (replaceOne eb)))
    Let (Cnd ec et ef)      eb   -> chg (Cnd ec (Let et eb) (Let ef eb))
    Let (V v)               eb   -> chg (sbs v eb)
    Let (NV v)         eb
      | isFresh eb               -> chg (sbs v eb)
    Let el             eb        -> Let  <$@> el <*@> eb

    Non                          -> pure Non

    Som (NV e)                   -> case TFG.getPrfHasSinMay t of
      PrfHasSin                  -> chg (Let e  (Som (Var Zro)))
    Som e                        -> case TFG.getPrfHasSinMay t of
      PrfHasSin                  -> Som  <$@> e

    May (NV em) en      es       -> chg (Let em (May (Var Zro)   (sucAll en) (sucAll es)))
    May (V  em) (NV en) es       -> chg (Let en (May (sucAll em) (Var Zro)   (sucAll es)))
    May (V  em) (V  en) (NV es)  -> chg (Let es (May (sucAll em) (sucAll en) (Var Zro)))
    May Non     en      _        -> chg en
    May (Som e) _       es       -> chg (App es e)
    May em      en      es       -> May  <$@> em <*@> en <*@> es

    Mul er  (NV ei)              -> chg (Let ei (Mul (sucAll er) (Var Zro)))
    Mul (NV er) (V ei)           -> chg (Let er (Mul (Var Zro)   (sucAll ei)))
    Mul er ei                    -> Mul  <$@> er <*@> ei

    Int i                        -> case t of
      TFG.Int                    -> chg (ConI i)
      TFG.Flt                    -> chg (ConF (fromIntegral i))
      _                          -> fail "Type Error3!"
