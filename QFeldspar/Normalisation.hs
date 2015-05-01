module QFeldspar.Normalisation (nrm) where

import QFeldspar.MyPrelude

import QFeldspar.Expression.GADTFirstOrder
import QFeldspar.Expression.Utils.GADTFirstOrder
    (sucAll,sbs,replaceOne,cntVar,pattern TF,pattern V,pattern NV)
import QFeldspar.Variable.Typed
import QFeldspar.Singleton
import QFeldspar.ChangeMonad
import QFeldspar.Expression.Utils.Common
import qualified QFeldspar.Type.GADT as TFG

nrm :: HasSin TFG.Typ a => Exp g a -> Exp g a
nrm = tilNotChg nrmOne

nrmOne :: forall g a. HasSin TFG.Typ a => Exp g a -> Chg (Exp g a)
nrmOne ee = let t = sin :: TFG.Typ a in case ee of
    App ef                      (NV ea) -> chg (Let ea (App (sucAll ef) (Var Zro)))
    App (TF (Abs eb))           (V  ea) -> chg (sbs ea eb)
    App (TF (Cnd (V ec) et ef)) (V  ea) -> chg (Cnd ec (App et ea) (App ef ea))
    App (TF (Let (NV el) eb))   (V  ea) -> chg (Let el (App eb (sucAll ea)))

    Cnd (NV ec)           et ef -> chg (Let ec (Cnd (Var Zro) (sucAll et) (sucAll ef)))
    Cnd (TF (ConB True))  et _  -> chg et
    Cnd (TF (ConB False)) _  ef -> chg ef

    Whl (NV ec) eb      ei      -> chg (Let ec (Whl (Var Zro) (sucAll eb) (sucAll ei)))
    Whl (V  ec) (NV eb) ei      -> chg (Let eb (Whl (sucAll ec) (Var Zro)  (sucAll ei)))
    Whl (V  ec) (V  eb) (NV ei) -> chg (Let ei (Whl (sucAll ec) (sucAll eb) (Var Zro)))

    Tpl (NV ef) es               -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin)    -> chg (Let ef (Tpl (Var Zro) (sucAll es)))
    Tpl (V ef)  (NV es)          -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin)    -> chg (Let es (Tpl (sucAll ef) (Var Zro)))

    Fst (NV e)                   -> chg (Let e (Fst (Var Zro)))
    Fst (TF (Tpl (V ef) (V _)))  -> chg  ef

    Snd (NV e)                   -> chg (Let e (Snd (Var Zro)))
    Snd (TF (Tpl (V _)  (V es))) -> chg  es

    Ary (NV el) ef               -> chg (Let el (Ary (Var Zro) (sucAll ef)))
    Ary (V  el) (NV ef)          -> case TFG.getPrfHasSinAry t of
      PrfHasSin                  -> chg (Let ef (Ary (sucAll el) (Var Zro)))

    Len (NV ea)                  -> chg (Let ea (Len (Var Zro)))
    Len (TF (Ary (V el) _))      -> chg  el

    Ind (NV ea)        ei        -> chg (Let ea (Ind (Var Zro) (sucAll ei)))
    Ind (V ea)         (NV ei)   -> chg (Let ei (Ind (sucAll ea) (Var Zro)))
    Ind (TF (Ary (V _) ef)) (V ei) -> chg (App ef ei)

    AryV (NV el) ef              -> chg (Let el (AryV (Var Zro) (sucAll ef)))
    AryV (V  el) (NV ef)         -> case TFG.getPrfHasSinVec t of
      PrfHasSin                  -> chg (Let ef (AryV (sucAll el) (Var Zro)))

    LenV (NV ea)                 -> chg (Let ea (LenV (Var Zro)))
    LenV (TF (AryV (V el) _))    -> chg  el

    IndV (NV ea)              ei      -> chg (Let ea (IndV (Var Zro)  (sucAll ei)))
    IndV (V ea)               (NV ei) -> chg (Let ei (IndV (sucAll ea) (Var Zro)))
    IndV (TF (AryV (V _) ef)) (V ei)  -> chg (App ef ei)

    Cmx (NV er) ei               -> chg (Let er (Cmx  (Var Zro)  (sucAll ei)))
    Cmx (V er)  (NV ei)          -> chg (Let ei (Cmx  (sucAll er) (Var Zro) ))

    Let (TF (Let (NV el') eb'))  eb   -> chg (Let el' (Let eb' (replaceOne eb)))
    Let (TF (Cnd ec et ef))      eb   -> chg (Cnd ec (Let et eb) (Let ef eb))
    Let (V v)               eb   -> chg (sbs v eb)
    Let (NV v)         eb
      | cntVar Zro eb == 0       -> chg (sbs v eb)

    Som (NV e)                   -> case TFG.getPrfHasSinMay t of
      PrfHasSin                  -> chg (Let e  (Som (Var Zro)))

    May (NV em) en      es       -> chg (Let em (May (Var Zro)   (sucAll en) (sucAll es)))
    May (V  em) (NV en) es       -> chg (Let en (May (sucAll em) (Var Zro)   (sucAll es)))
    May (V  em) (V  en) (NV es)  -> chg (Let es (May (sucAll em) (sucAll en) (Var Zro)))
    May (TF Non) en      _       -> chg en
    May (TF (Som e)) _       es  -> chg (App es e)

    Mul er  (NV ei)              -> chg (Let ei (Mul (sucAll er) (Var Zro)))
    Mul (NV er) (V ei)           -> chg (Let er (Mul (Var Zro)   (sucAll ei)))

    Add er  (NV ei)              -> chg (Let ei (Add (sucAll er) (Var Zro)))
    Add (NV er) (V ei)           -> chg (Let er (Add (Var Zro)   (sucAll ei)))

    Sub er  (NV ei)              -> chg (Let ei (Sub (sucAll er) (Var Zro)))
    Sub (NV er) (V ei)           -> chg (Let er (Sub (Var Zro)   (sucAll ei)))

    Eql er  (NV ei)              -> chg (Let ei (Eql (sucAll er) (Var Zro)))
    Eql (NV er) (V ei)           -> chg (Let er (Eql (Var Zro)   (sucAll ei)))

    Ltd er  (NV ei)              -> chg (Let ei (Ltd (sucAll er) (Var Zro)))
    Ltd (NV er) (V ei)           -> chg (Let er (Ltd (Var Zro)   (sucAll ei)))

    Int i                        -> case t of
      TFG.Wrd                    -> chg (ConI i)
      TFG.Flt                    -> chg (ConF (fromIntegral i))
      _                          -> fail "Type Error3!"

    Mem (NV e)                   -> chg (Let e (Mem (Var Zro)))

    _                            -> $(genOverloadedMW 'ee ''Exp  [] (trvWrp 't)
     (\ tt -> if
      | matchQ tt [t| Exp a a |] -> [| nrmOne |]
      | otherwise                -> [| pure   |]))
