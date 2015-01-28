{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Simplify  where

import QFeldspar.MyPrelude hiding (foldl,fmap)
import qualified QFeldspar.Expression.MiniFeldspar          as MF
import qualified QFeldspar.Expression.Utils.MiniFeldspar    as MF
-- import qualified QFeldspar.Expression.GADTHigherOrder       as GHO
-- import qualified QFeldspar.Expression.Utils.GADTHigherOrder as GHO

import QFeldspar.Singleton
import qualified QFeldspar.Type.GADT as TFG
import QFeldspar.Expression.Utils.Common
import QFeldspar.ChangeMonad

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
         SmpOne (MF.Exp n t) where
  smpOne ee = let t = sin :: TFG.Typ t in case ee of
    MF.AppV x es -> MF.AppV x <$> TFG.mapMC (sinTyp x) smpOne es
    MF.Let ea   eb
      | MF.hasOneOrZro eb -> chg (eb ea)
    MF.Ary el ef -> case TFG.getPrfHasSinAry t of
      PrfHasSin -> case el of
        MF.TF (MF.Len (e :: MF.Exp n (Ary te))) -> let v = genNewNam "__smpOneAry__"
                                                       {-# NOINLINE v #-}
                                                   in deepseq v $ case ef (MF.Tmp v) of
           MF.TF (MF.Ind (e' :: MF.Exp n (Ary te')) (MF.Tmp m))
            | m == v -> case eqlSin (sin :: TFG.Typ te) (sin :: TFG.Typ te') of
               Rgt Rfl -> do if MF.eql e e'
                             then chg e
                             else MF.Ary <$@> el <*@> ef
               _       -> MF.Ary <$@> el <*@> ef
           _           -> MF.Ary <$@> el <*@> ef
        _              -> MF.Ary <$@> el <*@> ef
    _ -> $(genOverloadedMW 'ee ''MF.Exp  ['MF.AppV,'MF.Ary]
                               (trvWrp 't)
           (\ tt -> if
                | matchQ tt [t| MF.Exp t t -> MF.Exp t t |] -> [| smpOne |]
                | matchQ tt [t| MF.Exp t t |]               -> [| smpOne |]
                | otherwise                                 -> [| pure   |]))

instance (HasSin TFG.Typ tb, HasSin TFG.Typ ta) =>
         SmpOne (MF.Exp n ta -> MF.Exp n tb) where
  smpOne f = let v = genNewNam "__SmpOneF__"
                 {-# NOINLINE v #-}
             in deepseq v $ do f' <- smpOne (f (MF.Tmp v))
                               return (\ x -> MF.absTmp x v f')
