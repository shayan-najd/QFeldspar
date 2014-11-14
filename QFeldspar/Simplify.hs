module QFeldspar.Simplify  where

import QFeldspar.MyPrelude hiding (foldl,fmap)

import QFeldspar.Expression.Feldspar.MiniFeldspar
import QFeldspar.Expression.Feldspar.Utils.MiniFeldspar(eql,hasOneOrZro,absTmp,pattern TF)

import QFeldspar.Singleton
import qualified QFeldspar.Type.Feldspar.GADT as TFG
import QFeldspar.ChangeMonad

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
    ConI i                       -> pure (ConI i)
    ConB b                       -> pure (ConB b)
    ConF f                       -> pure (ConF f)
    AppV x             es        -> AppV x <$> TFG.mapMC (sinTypOf x t) smpOne es
    Cnd ec           et ef       -> Cnd  <$@> ec <*@> et <*@> ef
    Whl ec eb ei                 -> Whl  <$@> ec <*@> eb <*@> ei
    Tpl ef      es               -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin)    -> Tpl  <$@> ef <*@> es
    Fst e                        -> Fst  <$@> e
    Snd e                        -> Snd  <$@> e
    Ary el      ef               -> case TFG.getPrfHasSinAry t of
      PrfHasSin                  -> case el of
        TF (Len (e :: Exp n (Ary te)))->
              let v = genNewNam "__smpOneAry__"
                  {-# NOINLINE v #-}
              in deepseq v $ case ef (Tmp v) of
          TF (Ind (e' :: Exp n (Ary te')) (Tmp m))
            | m == v -> case eqlSin (sin :: TFG.Typ te) (sin :: TFG.Typ te') of
               Rgt Rfl
                   | eql e e'    -> chg e
               _                 -> Ary  <$@> el <*@> ef
          _                      -> Ary  <$@> el <*@> ef
        _                        -> Ary  <$@> el <*@> ef
    Len e                        -> Len  <$@> e
    Ind ea             ei        -> Ind  <$@> ea <*@> ei
    Cmx er ei                    -> Cmx  <$@> er <*@> ei

    Let ea         eb
      | hasOneOrZro eb           -> chg (eb ea)
    Let el             eb        -> Let  <$@> el <*@> eb
    Tmp x                        -> pure (Tmp x)
    Tag x e                      -> Tag x <$@> e
    Mul er ei                    -> Mul   <$@> er <*@> ei

instance (HasSin TFG.Typ tb, HasSin TFG.Typ ta) =>
         SmpOne (Exp n ta -> Exp n tb) where
  smpOne f = let v = genNewNam "__SmpOneF__"
                 {-# NOINLINE v #-}
             in deepseq v $ do eb <- smpOne (f (Tmp v))
                               return (\ x -> absTmp x v eb)
