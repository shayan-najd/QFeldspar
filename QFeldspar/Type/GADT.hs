{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Type.GADT where

import QFeldspar.MyPrelude
import qualified QFeldspar.Environment.Typed as ET
import QFeldspar.Singleton

data Typ :: * -> * where
  Wrd :: Typ Word32
  Bol :: Typ Bool
  Flt :: Typ Float
  Arr :: Typ ta -> Typ tb -> Typ (ta -> tb)
  Tpl :: Typ tf -> Typ ts -> Typ (tf , ts)
  Ary :: Typ t  -> Typ (Ary t)
  Vct :: Typ t  -> Typ (Vec t)
  May :: Typ t  -> Typ (Maybe t)
  Cmx :: Typ (Complex Float)

deriving instance Show (Typ t)

type Type a = HasSin Typ a
type Types as = HasSin (ET.Env Typ) as

instance HasSin Typ Word32 where
  sin = Wrd

instance HasSin Typ Bool where
  sin = Bol

instance HasSin Typ Float where
  sin = Flt

instance (Type ta , Type tb) => HasSin Typ (ta -> tb) where
  sin = Arr sin sin

instance (Type tf , Type ts) => HasSin Typ (tf , ts) where
  sin = Tpl sin sin

instance Type ta => HasSin Typ (Ary ta) where
  sin = Ary sin

instance Type ta => HasSin Typ (Vec ta) where
  sin = Vct sin

instance Type ta => HasSin Typ (Maybe ta) where
  sin = May sin

instance HasSin Typ (Complex Float) where
  sin = Cmx

instance EqlSin Typ where
  eqlSin Wrd         Wrd           = return Rfl
  eqlSin Bol         Bol           = return Rfl
  eqlSin Flt         Flt           = return Rfl
  eqlSin (Arr ta tb) (Arr ta' tb') = do Rfl <- eqlSin ta ta'
                                        Rfl <- eqlSin tb tb'
                                        return Rfl
  eqlSin (Tpl tf ts) (Tpl tf' ts') = do Rfl <- eqlSin tf tf'
                                        Rfl <- eqlSin ts ts'
                                        return Rfl
  eqlSin (Ary t)     (Ary t')      = do Rfl <- eqlSin t t'
                                        return Rfl
  eqlSin (Vct t)     (Vct t')      = do Rfl <- eqlSin t t'
                                        return Rfl
  eqlSin (May t)     (May t')      = do Rfl <- eqlSin t t'
                                        return Rfl
  eqlSin Cmx         Cmx           = return Rfl
  eqlSin _              _          = fail "Type Error!"

instance GetPrfHasSin Typ where
  getPrfHasSin t  = case t of
    Wrd       -> PrfHasSin
    Bol       -> PrfHasSin
    Flt       -> PrfHasSin
    Arr ta tb -> case (getPrfHasSin ta , getPrfHasSin tb) of
      (PrfHasSin , PrfHasSin) -> PrfHasSin
    Tpl tf ts -> case (getPrfHasSin tf , getPrfHasSin ts) of
      (PrfHasSin , PrfHasSin) -> PrfHasSin
    Ary ta    -> case getPrfHasSin ta of
      PrfHasSin -> PrfHasSin
    Vct ta    -> case getPrfHasSin ta of
      PrfHasSin -> PrfHasSin
    May ta    -> case getPrfHasSin ta of
      PrfHasSin -> PrfHasSin
    Cmx       -> PrfHasSin

getPrfHasSinArr :: forall ta tb t. Type (ta -> tb) =>
                   t (ta -> tb) -> (PrfHasSin Typ ta , PrfHasSin Typ tb)
getPrfHasSinArr _ = case sin :: Typ (ta -> tb) of
  Arr ta tb -> (getPrfHasSin ta , getPrfHasSin tb)

getPrfHasSinTpl :: forall tf ts t. Type (tf , ts) =>
                   t (tf , ts) -> (PrfHasSin Typ tf , PrfHasSin Typ ts)
getPrfHasSinTpl _ = case sin :: Typ (tf , ts) of
  Tpl tf ts -> (getPrfHasSin tf , getPrfHasSin ts)

getPrfHasSinAry :: forall ta t. Type (Ary ta) =>
                   t (Ary ta) -> PrfHasSin Typ ta
getPrfHasSinAry _ = case sin :: Typ (Ary ta) of
  Ary ta    -> getPrfHasSin ta

getPrfHasSinVec :: forall ta t. Type (Vec ta) =>
                   t (Vec ta) -> PrfHasSin Typ ta
getPrfHasSinVec _ = case sin :: Typ (Vec ta) of
  Vct ta    -> getPrfHasSin ta


getPrfHasSinMay :: forall ta t. Type (Maybe ta) =>
                   t (Maybe ta) -> PrfHasSin Typ ta
getPrfHasSinMay _ = case sin :: Typ (Maybe ta) of
  May ta    -> getPrfHasSin ta

getPrfHasSinArrM :: Type (ta -> tb) =>
                    t (ta -> tb) -> ErrM (PrfHasSin Typ ta , PrfHasSin Typ tb)
getPrfHasSinArrM = return . getPrfHasSinArr

getPrfHasSinTplM :: Type (tf , ts) =>
                    t (tf , ts) -> ErrM (PrfHasSin Typ tf , PrfHasSin Typ ts)
getPrfHasSinTplM = return . getPrfHasSinTpl

getPrfHasSinAryM :: Type (Ary ta) =>
                   t (Ary ta) -> ErrM (PrfHasSin Typ ta)
getPrfHasSinAryM = return  . getPrfHasSinAry

getPrfHasSinVecM :: Type (Vec ta) =>
                   t (Vec ta) -> ErrM (PrfHasSin Typ ta)
getPrfHasSinVecM = return  . getPrfHasSinVec

getPrfHasSinMayM :: Type (Maybe ta) =>
                   t (Maybe ta) -> ErrM (PrfHasSin Typ ta)
getPrfHasSinMayM = return  . getPrfHasSinMay

mapC :: Types as =>
        (forall a. Type a => f a -> f' a) ->
        ET.Env f as -> ET.Env f' as
mapC f xss = case xss of
  ET.Emp      -> ET.Emp
  ET.Ext x xs -> case getPrfHasSinEnvOf xss of
    (PrfHasSin,PrfHasSin) -> ET.Ext (f x) (mapC f xs)

mapMC :: (Applicative m , Types as) =>
         (forall a. Type a => f a -> m (f' a)) ->
         ET.Env f as -> m (ET.Env f' as)
mapMC f xss = case xss of
  ET.Emp      -> pure ET.Emp
  ET.Ext x xs -> case getPrfHasSinEnvOf xss of
    (PrfHasSin,PrfHasSin) -> ET.Ext <$> f x <*> mapMC f xs

fld :: Types as => (forall a. Type a => b -> f a -> b) -> b ->
       ET.Env f as -> b
fld f z xss = case xss of
  ET.Emp      -> z
  ET.Ext e es -> case getPrfHasSinEnvOf xss of
   (PrfHasSin , PrfHasSin) -> f (fld f z es) e

getArgTyp :: Typ (ta -> tb) -> Typ ta
getArgTyp (Arr ta _) = ta

getBdyTyp :: Typ (ta -> tb) -> Typ tb
getBdyTyp (Arr _ tb) = tb

getFstTyp :: Typ (tf , ts) -> Typ tf
getFstTyp (Tpl tf _) = tf

getSndTyp :: Typ (tf , ts) -> Typ ts
getSndTyp (Tpl _  ts) = ts

getAryTyp :: Typ (Ary ta) -> Typ ta
getAryTyp (Ary ta) = ta

getVecTyp :: Typ (Vec ta) -> Typ ta
getVecTyp (Vct ta) = ta

getMayTyp :: Typ (Maybe ta) -> Typ ta
getMayTyp (May ta) = ta

type family (:->) a b where
  '[]       :-> b = b
  (a ': as) :-> b = a -> (as :-> b)

cur :: ET.Env Typ as -> Typ a -> Typ (as :-> a)
cur ET.Emp        b = b
cur (ET.Ext a as) b = Arr a (cur as b)

getPrfHasSinEnv :: forall a as. Types (a ': as) => (PrfHasSin Typ a, PrfHasSin (ET.Env Typ) as)
getPrfHasSinEnv = case sin :: ET.Env Typ (a ': as) of
  ET.Ext a as  -> (getPrfHasSin a , getPrfHasSin as)

getPrfHasSinEnvOf :: forall a as f. Types (a ': as) =>
                     ET.Env f (a ': as) -> (PrfHasSin Typ a, PrfHasSin (ET.Env Typ) as)
getPrfHasSinEnvOf _ = getPrfHasSinEnv
