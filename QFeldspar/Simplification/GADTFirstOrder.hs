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

smp :: SmpOne a => a -> a
smp = tilNotChg smpOne

class SmpOne a where
  smpOne :: a -> Chg a

infixl 4 <$@>
(<$@>) :: SmpOne a => (a -> b) -> a -> Chg b
el <$@> er = el <$> smpOne er

infixl 4 <*@>
(<*@>) :: SmpOne a => Chg (a -> b) -> a -> Chg b
el <*@> er = el <*> smpOne er

instance (HasSin TFG.Typ t) =>
         SmpOne (Exp n t) where
  smpOne ee = let t = sin :: TFG.Typ t in case ee of
    Let m n
      | cntVar Zro n <= 1 -> chg (sbs m n)
    Ary el ef       -> case TFG.getPrfHasSinAry t of
      PrfHasSin     -> case el of
        Len (e :: Exp n (Ary te)) -> case ef of
          Abs (Ind (e' :: Exp (Int ': n) (Ary te')) (Var Zro)) -> case eqlSin (sin :: TFG.Typ te) (sin :: TFG.Typ te') of
            Rgt Rfl -> do if eql (sucAll e) e'
                          then chg e
                          else Ary <$@> el <*@> ef
            _       -> Ary <$@> el <*@> ef
          _         -> Ary <$@> el <*@> ef
        _           -> Ary <$@> el <*@> ef
    _       -> $(genOverloadedMW 'ee ''Exp  [] (trvWrp 't)
      (\ tt -> if
           | matchQ tt [t| Exp t t |]            -> [| smpOne |]
           | otherwise                           -> [| pure   |]))


{-
instance (HasSin TFG.Typ t) =>
         SmpOne (Exp n t) where
  smpOne ee = let t = sin :: TFG.Typ t in case ee of
    AppV x es -> AppV x <$> TFG.mapMC (sinTyp x) smpOne es
    Let ea   eb
      | hasOneOrZro eb -> chg (eb ea)
    Ary el ef -> case TFG.getPrfHasSinAry t of
      PrfHasSin -> case el of
        TF (Len (e :: Exp n (Ary te))) -> let v = genNewNam "__smpOneAry__"
                                                       {-# NOINLINE v #-}
                                                   in deepseq v $ case ef (Tmp v) of
           TF (Ind (e' :: Exp n (Ary te')) (Tmp m))
            | m == v -> case eqlSin (sin :: TFG.Typ te) (sin :: TFG.Typ te') of
               Rgt Rfl -> do if eql e e'
                             then chg e
                             else Ary <$@> el <*@> ef
               _       -> Ary <$@> el <*@> ef
           _           -> Ary <$@> el <*@> ef
        _              -> Ary <$@> el <*@> ef
    _ -> $(genOverloadedMW 'ee ''Exp  ['AppV,'Ary]
                               (trvWrp 't)
           (\ tt -> if
                | matchQ tt [t| Exp t t -> Exp t t |] -> [| smpOne |]
                | matchQ tt [t| Exp t t |]               -> [| smpOne |]
                | otherwise                                 -> [| pure   |]))

instance (HasSin TFG.Typ tb, HasSin TFG.Typ ta) =>
         SmpOne (Exp n ta -> Exp n tb) where
  smpOne f = let v = genNewNam "__SmpOneF__"
                 {-# NOINLINE v #-}
             in deepseq v $ do f' <- smpOne (f (Tmp v))
                               return (\ x -> absTmp x v f')
-}
