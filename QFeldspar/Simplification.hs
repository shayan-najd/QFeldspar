{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Simplification (smp) where

import QFeldspar.MyPrelude

import QFeldspar.Expression.GADTFirstOrder
import QFeldspar.Expression.Utils.Equality.GADTFirstOrder
import QFeldspar.Expression.Utils.GADTFirstOrder
    (sucAll,sbs,cntVar)
import QFeldspar.Variable.Typed
import QFeldspar.Singleton
import QFeldspar.ChangeMonad
import QFeldspar.Expression.Utils.Common
import qualified QFeldspar.Type.GADT as TG

smp :: TG.Type a => Exp s g a -> Exp s g a
smp = tilNotChg smpOne

smpOne :: forall s g a. TG.Type a =>
          Exp s g a -> Chg (Exp s g a)
smpOne ee = let t = sin :: TG.Typ a in case ee of
    Prm x ns -> Prm x <$> TG.mapMC smpOne ns
    LeT m n
      | cntVar Zro n <= 1 -> chg (sbs m n)
    Cnd _ m n
      | eql m n     -> chg m
    Ary el ef       -> case TG.getPrfHasSinAry t of
      PrfHasSin     -> case el of
        Len (e :: Exp s g (Ary te)) -> case ef of
          Abs (Ind (e' :: Exp s (Word32 ': g) (Ary te')) (Var Zro)) -> case eqlSin (sin :: TG.Typ te) (sin :: TG.Typ te') of
            Rgt Rfl -> do if eql (sucAll e) e'
                          then chg e
                          else Ary <$> smpOne el <*> smpOne ef
            _       -> Ary <$> smpOne el <*> smpOne ef
          _         -> Ary <$> smpOne el <*> smpOne ef
        _           -> Ary <$> smpOne el <*> smpOne ef
    _       -> $(genOverloadedMW 'ee ''Exp  ['Prm] (trvWrp 't)
      (\ tt -> if
           | matchQ tt [t| Exp a a a |] -> [| smpOne |]
           | otherwise                  -> [| pure   |]))
