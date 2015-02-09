module QFeldspar.Simplification.GADTFirstOrder (smp) where

import QFeldspar.MyPrelude

import QFeldspar.Expression.GADTFirstOrder
import QFeldspar.Expression.Utils.GADTFirstOrder
    (sucAll,sbs,cntVar,eql)
import QFeldspar.Variable.Typed
import QFeldspar.Singleton
import QFeldspar.ChangeMonad
import QFeldspar.Expression.Utils.Common
import qualified QFeldspar.Type.GADT as TFG

smp :: HasSin TFG.Typ a => Exp g a -> Exp g a
smp = tilNotChg smpOne

smpOne :: forall g a. HasSin TFG.Typ a =>
          Exp g a -> Chg (Exp g a)
smpOne ee = let t = sin :: TFG.Typ a in case ee of
    Let m n
      | cntVar Zro n <= 1 -> chg (sbs m n)
    Ary el ef       -> case TFG.getPrfHasSinAry t of
      PrfHasSin     -> case el of
        Len (e :: Exp g (Ary te)) -> case ef of
          Abs (Ind (e' :: Exp (Int ': g) (Ary te')) (Var Zro)) -> case eqlSin (sin :: TFG.Typ te) (sin :: TFG.Typ te') of
            Rgt Rfl -> do if eql (sucAll e) e'
                          then chg e
                          else Ary <$> smpOne el <*> smpOne ef
            _       -> Ary <$> smpOne el <*> smpOne ef
          _         -> Ary <$> smpOne el <*> smpOne ef
        _           -> Ary <$> smpOne el <*> smpOne ef
    _       -> $(genOverloadedMW 'ee ''Exp  [] (trvWrp 't)
      (\ tt -> if
           | matchQ tt [t| Exp a a |]            -> [| smpOne |]
           | otherwise                           -> [| pure   |]))
