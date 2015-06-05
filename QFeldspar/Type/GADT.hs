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

instance (HasSin Typ ta , HasSin Typ tb) => HasSin Typ (ta -> tb) where
  sin = Arr sin sin

instance (HasSin Typ tf , HasSin Typ ts) => HasSin Typ (tf , ts) where
  sin = Tpl sin sin

instance HasSin Typ ta => HasSin Typ (Ary ta) where
  sin = Ary sin

instance HasSin Typ ta => HasSin Typ (Vec ta) where
  sin = Vct sin

instance HasSin Typ ta => HasSin Typ (Maybe ta) where
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

getPrfHasSinArr :: forall ta tb t. HasSin Typ (ta -> tb) =>
                   t (ta -> tb) -> (PrfHasSin Typ ta , PrfHasSin Typ tb)
getPrfHasSinArr _ = case sin :: Typ (ta -> tb) of
  Arr ta tb -> (getPrfHasSin ta , getPrfHasSin tb)

getPrfHasSinTpl :: forall tf ts t. HasSin Typ (tf , ts) =>
                   t (tf , ts) -> (PrfHasSin Typ tf , PrfHasSin Typ ts)
getPrfHasSinTpl _ = case sin :: Typ (tf , ts) of
  Tpl tf ts -> (getPrfHasSin tf , getPrfHasSin ts)

getPrfHasSinAry :: forall ta t. HasSin Typ (Ary ta) =>
                   t (Ary ta) -> PrfHasSin Typ ta
getPrfHasSinAry _ = case sin :: Typ (Ary ta) of
  Ary ta    -> getPrfHasSin ta

getPrfHasSinVec :: forall ta t. HasSin Typ (Vec ta) =>
                   t (Vec ta) -> PrfHasSin Typ ta
getPrfHasSinVec _ = case sin :: Typ (Vec ta) of
  Vct ta    -> getPrfHasSin ta


getPrfHasSinMay :: forall ta t. HasSin Typ (Maybe ta) =>
                   t (Maybe ta) -> PrfHasSin Typ ta
getPrfHasSinMay _ = case sin :: Typ (Maybe ta) of
  May ta    -> getPrfHasSin ta

getPrfHasSinArrM :: HasSin Typ (ta -> tb) =>
                    t (ta -> tb) -> ErrM (PrfHasSin Typ ta , PrfHasSin Typ tb)
getPrfHasSinArrM = return . getPrfHasSinArr

getPrfHasSinTplM :: HasSin Typ (tf , ts) =>
                    t (tf , ts) -> ErrM (PrfHasSin Typ tf , PrfHasSin Typ ts)
getPrfHasSinTplM = return . getPrfHasSinTpl

getPrfHasSinAryM :: HasSin Typ (Ary ta) =>
                   t (Ary ta) -> ErrM (PrfHasSin Typ ta)
getPrfHasSinAryM = return  . getPrfHasSinAry

getPrfHasSinVecM :: HasSin Typ (Vec ta) =>
                   t (Vec ta) -> ErrM (PrfHasSin Typ ta)
getPrfHasSinVecM = return  . getPrfHasSinVec

getPrfHasSinMayM :: HasSin Typ (Maybe ta) =>
                   t (Maybe ta) -> ErrM (PrfHasSin Typ ta)
getPrfHasSinMayM = return  . getPrfHasSinMay

type family Out (t :: *) :: * where
  Out (ta -> tb) = Out tb
  Out t          = t

data EqlOut :: * -> * -> * where
  EqlOut :: EqlOut (Out t2) t2

eqlOut :: Typ t1 -> Typ t2 -> ErrM (EqlOut t1 t2)
eqlOut Wrd         Wrd           = return EqlOut
eqlOut Bol         Bol           = return EqlOut
eqlOut Flt         Flt           = return EqlOut
eqlOut t           (Arr _ tb)    = do EqlOut <- eqlOut t tb
                                      return EqlOut
eqlOut (Ary ta)    (Ary ta')     = do Rfl <- eqlSin ta ta'
                                      return EqlOut
eqlOut (Vct ta)    (Vct ta')     = do Rfl <- eqlSin ta ta'
                                      return EqlOut
eqlOut (May ta)    (May ta')     = do Rfl <- eqlSin ta ta'
                                      return EqlOut
eqlOut (Tpl tf ts) (Tpl tf' ts') = do Rfl <- eqlSin tf tf'
                                      Rfl <- eqlSin ts ts'
                                      return EqlOut
eqlOut Cmx         Cmx           = return EqlOut
eqlOut _              _          = fail "Normalisation Error!"

type family Arg (t :: *) :: [*] where
  Arg (ta -> tb) = ta ': Arg tb
  Arg t          = '[]

data EqlArg :: [*] -> * -> * where
  EqlArg :: EqlArg (Arg t2) t2

eqlArg :: ET.Env Typ r -> Typ t -> ErrM (EqlArg r t)
eqlArg ET.Emp         Wrd          = return EqlArg
eqlArg ET.Emp         Bol          = return EqlArg
eqlArg ET.Emp         Flt          = return EqlArg
eqlArg (ET.Ext ta ts) (Arr ta' tb) = do Rfl    <- eqlSin ta ta'
                                        EqlArg <- eqlArg ts tb
                                        return EqlArg
eqlArg ET.Emp         (Ary _)      = return EqlArg
eqlArg ET.Emp         (Vct _)      = return EqlArg
eqlArg ET.Emp         (May _)      = return EqlArg
eqlArg ET.Emp         (Tpl _ _)    = return EqlArg
eqlArg ET.Emp         Cmx          = return EqlArg
eqlArg _              _            = fail "Normalisation Error!"

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

fld :: Types as => (forall a. HasSin Typ a => b -> f a -> b) -> b ->
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

getOutTyp :: forall a. Typ a -> Typ (Out a)
getOutTyp (Arr _ b) = getOutTyp b
getOutTyp a         = case obvious :: a :~: Out a of
  Rfl              -> a

prfHasSinOut :: forall t. PrfHasSin Typ t -> PrfHasSin Typ (Out t)
prfHasSinOut PrfHasSin = case sin :: Typ t of
  Wrd       -> PrfHasSin
  Bol       -> PrfHasSin
  Flt       -> PrfHasSin
  Arr ta tb -> case (getPrfHasSin ta , getPrfHasSin tb) of
    (PrfHasSin , y@PrfHasSin) -> prfHasSinOut y
  Tpl tf ts -> case (getPrfHasSin tf , getPrfHasSin ts) of
    (PrfHasSin , PrfHasSin) -> PrfHasSin
  Ary ta    -> case getPrfHasSin ta of
    PrfHasSin -> PrfHasSin
  Vct ta    -> case getPrfHasSin ta of
    PrfHasSin -> PrfHasSin
  May ta    -> case getPrfHasSin ta of
    PrfHasSin -> PrfHasSin
  Cmx       -> PrfHasSin

getTypTailEnv :: forall t ra rb tf.
                 (Arg t ~ Add ra rb) =>
                 Typ t -> ET.Env tf ra -> ET.Env tf rb -> ExsSin Typ
getTypTailEnv t          ET.Emp        _ = ExsSin t
getTypTailEnv (Arr _ ts) (ET.Ext _ rs) r = getTypTailEnv ts rs r
getTypTailEnv _          _             _ = impossible

getSinDiff :: (Arg t ~ Add ra rb) =>
              Typ t -> ET.Env tf ra -> ET.Env tf rb -> ET.Env Typ rb
getSinDiff _          _             ET.Emp          = ET.Emp
getSinDiff (Arr _ t)  (ET.Ext _ ra) rb@(ET.Ext _ _) = getSinDiff t ra rb
getSinDiff (Arr t ts) ET.Emp        (ET.Ext _ rb)   = ET.Ext t (getSinDiff ts ET.Emp rb)
getSinDiff _          _             _               = impossible

getPrf :: (Arg t ~ Add ra (tb ': rb)) =>
          Typ t -> ET.Env tf ra -> ET.Env tf (tb ': rb) -> PrfHasSin Typ tb
getPrf Wrd                   _          _           = impossible
getPrf Bol                   _          _           = impossible
getPrf Flt                   _          _           = impossible
getPrf (Tpl _ _)             _          _           = impossible
getPrf (Ary _)               _          _           = impossible
getPrf (Vct _)               _          _           = impossible
getPrf Cmx                   _          _           = impossible
getPrf (May _)               _          _           = impossible
getPrf (Arr t Wrd)       ET.Emp        (ET.Ext _ ET.Emp) = getPrfHasSin t
getPrf (Arr _ Wrd)       _          _           = impossible
getPrf (Arr t Bol)       ET.Emp        (ET.Ext _ ET.Emp) = getPrfHasSin t
getPrf (Arr _ Bol)       _          _           = impossible
getPrf (Arr t Flt)       ET.Emp        (ET.Ext _ ET.Emp) = getPrfHasSin t
getPrf (Arr _ Flt)       _          _           = impossible
getPrf (Arr t (Tpl _ _)) ET.Emp        (ET.Ext _ ET.Emp) = getPrfHasSin t
getPrf (Arr _ (Tpl _ _)) _          _           = impossible
getPrf (Arr t (Ary _))   ET.Emp        (ET.Ext _ ET.Emp) = getPrfHasSin t
getPrf (Arr _ (Ary _))   _          _           = impossible
getPrf (Arr t (Vct _))   ET.Emp        (ET.Ext _ ET.Emp) = getPrfHasSin t
getPrf (Arr _ (Vct _))   _          _           = impossible
getPrf (Arr t Cmx)       ET.Emp        (ET.Ext _ ET.Emp) = getPrfHasSin t
getPrf (Arr _ Cmx)       _          _           = impossible
getPrf (Arr t (May _))   ET.Emp        (ET.Ext _ ET.Emp) = getPrfHasSin t
getPrf (Arr _ (May _))   _          _           = impossible
getPrf (Arr t (Arr _ _)) ET.Emp        (ET.Ext _ _)   = getPrfHasSin t
getPrf (Arr _ ts@(Arr _ _)) (ET.Ext _ es) es'      = getPrf ts es es'

type family (:->) a b where
  '[]       :-> b = b
  (a ': as) :-> b = a -> (as :-> b)

cur :: ET.Env Typ as -> Typ a -> Typ (as :-> a)
cur ET.Emp        b = b
cur (ET.Ext a as) b = Arr a (cur as b)

getPrfHasSinEnv :: forall a as. HasSin (ET.Env Typ) (a ': as) => (PrfHasSin Typ a, PrfHasSin (ET.Env Typ) as)
getPrfHasSinEnv = case sin :: ET.Env Typ (a ': as) of
  ET.Ext a as  -> (getPrfHasSin a , getPrfHasSin as)

getPrfHasSinEnvOf :: forall a as f. Types (a ': as) =>
                     ET.Env f (a ': as) -> (PrfHasSin Typ a, PrfHasSin (ET.Env Typ) as)
getPrfHasSinEnvOf _ = getPrfHasSinEnv
