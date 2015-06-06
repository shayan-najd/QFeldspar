{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-max-relevant-binds #-}
module QFeldspar.CSE where

import QFeldspar.Expression.GADTFirstOrder
import QFeldspar.Expression.Utils.Equality.GADTFirstOrder
import QFeldspar.Expression.Utils.GADTFirstOrder(prdAllM,sucAll,isVal)
import QFeldspar.Expression.Utils.Common

import QFeldspar.MyPrelude hiding (foldl,fmap)

import qualified QFeldspar.Type.GADT as TG
import QFeldspar.ChangeMonad
import QFeldspar.Singleton
import QFeldspar.Variable.Typed
import QFeldspar.Environment.Typed

-- type family Env a :: [*]
-- type instance Env (Exp s g a) = g

cse :: TG.Type a => Exp s g a -> Exp s g a
cse = tilNotChg cseOne

cseOne :: forall s g a. TG.Type a => Exp s g a -> Chg (Exp s g a)
cseOne ee = let t = sin :: TG.Typ a in case ee of
  ConI i                    -> pure (ConI i)
  ConB b                    -> pure (ConB b)
  ConF f                    -> pure (ConF f)
  Prm  x es                 -> cseOneEnv x Emp es
  Var x                     -> pure (Var x)
  Abs eb                    -> case TG.getPrfHasSinArr t of
    (PrfHasSin , PrfHasSin) -> Abs <$> cseOne eb
  App ef ea                 -> cseOne2 App ef ea
  Cnd ec et ef              -> cseOne3 Cnd ec et ef
  Whl ec eb ei              -> cseOne3 Whl ec eb ei
  Tpl ef es                 -> case TG.getPrfHasSinTpl t of
    (PrfHasSin , PrfHasSin) -> cseOne2 Tpl ef es
  Fst e                     -> Fst <$> cseOne e
  Snd e                     -> Snd <$> cseOne e
  Ary el ef                 -> case TG.getPrfHasSinAry t of
    PrfHasSin               -> cseOne2 Ary el ef
  Len e                     -> Len <$> cseOne e
  Ind ea ei                 -> cseOne2 Ind ea ei
  LeT el eb                 -> cseOne2F LeT el eb
  Cmx er ei                 -> cseOne2 Cmx er ei
  Tag x e                   -> Tag x <$> cseOne e
  Mul er ei                 -> cseOne2 Mul er ei
  Add er ei                 -> cseOne2 Add er ei
  Sub er ei                 -> cseOne2 Sub er ei
  Eql er ei                 -> cseOne2 Eql er ei
  Ltd er ei                 -> cseOne2 Ltd er ei
  Non                       -> pure Non
  Som eb                    -> case TG.getPrfHasSinMay t of
    PrfHasSin               -> Som <$> cseOne eb
  May l m n                 -> cseOne3 May l m n
  AryV m n                  -> case TG.getPrfHasSinVec t of
    PrfHasSin               -> cseOne2 AryV m n
  LenV n                    -> LenV <$> cseOne n
  IndV m n                  -> cseOne2 IndV m n
  Int  i                    -> pure (Int i)
  Mem  m                    -> Mem <$> cseOne m

remTag :: forall s g a. Exp s g a -> Exp s g a
remTag ee = case ee of
  Tag _  e  -> remTag e
  _         -> $(genOverloaded 'ee ''Exp  ['Tag]
   (\ tt -> if
    | matchQ tt [t| Exp a a a |] -> [| remTag |]
    | matchQ tt [t| Env (Exp a a) a |] -> [| fmap remTag |]
    | otherwise                  -> [| id |]))

remTheTag :: forall s g a. String -> Exp s g a -> Exp s g a
remTheTag x ee = case ee of
  Tag y e
      | x == y    -> e
      | otherwise -> Tag y (remTheTag x e)
  _               -> $(genOverloaded 'ee ''Exp  ['Tag]
   (\ tt -> if
    | matchQ tt [t| Exp a a a |] -> [| remTheTag  x |]
    | matchQ tt [t| Env (Exp a a) a |] -> [| fmap (remTheTag x) |]
    | otherwise                  -> [| id |]))

hasSubtermEnv :: (TG.Type b , TG.Types d) => Exp s g b -> Env (Exp s g) d -> Bool
hasSubtermEnv ex = TG.fld (\ b e -> b || hasSubterm ex e) False


cseOneEnv :: forall s g a d d' as.
       (TG.Type a , TG.Types d , TG.Types d' , as ~ Add d d') =>
       Var s (as TG.:-> a) -> Env (Exp s g) d -> Env (Exp s g) d' ->
                           Chg (Exp s g a)
cseOneEnv x d d' = do
  let tsd  = sin :: Env TG.Typ d
  let tsd' = sin :: Env TG.Typ d'
  PrfHasSin <- getPrfHasSinM (add tsd tsd')
  case d' of
    Emp           -> Prm x <$> TG.mapMC cseOne (add d d')
    Ext (e :: Exp s g te) (es :: Env (Exp s g) tes) -> case TG.getPrfHasSinEnvOf d' of
     (PrfHasSin,PrfHasSin) -> case obvious :: Add (Add d (te ': '[])) tes :~: Add d (te ': tes) of
      Rfl         -> let f tss = case tss of
                           []                -> do PrfHasSin <- getPrfHasSinM (add tsd (Ext (sinTyp e) Emp))
                                                   cseOneEnv x (add d (Ext e Emp)) es
                           (Exs1 ex tx : ts) -> case getPrfHasSin tx of
                             PrfHasSin       -> if hasSubtermEnv ex es
                                                then chg (LeT ex (Prm x (TG.mapC (\ ee -> absSubterm (Var Zro) (sucAll ex) (sucAll ee))
                                                                         (add d d'))))
                                                else f ts
                     in f (findSubterms e)

cseOne3 :: (TG.Type a , TG.Type b , TG.Type c , TG.Type d) =>
           (Exp s g a -> Exp s g b -> Exp s g c -> Exp s g d) ->
           Exp s g a -> Exp s g b -> Exp s g c -> Chg (Exp s g d)
cseOne3 k l m n = let fm tss = case tss of
                        [] ->  k <$> cseOne l <*> cseOne m <*> cseOne n
                        (Exs1 ex tx : ts) -> case getPrfHasSin tx of
                                      PrfHasSin -> if hasSubterm ex n
                                                   then chg (LeT ex (absSubterm (Var Zro) (sucAll ex) (sucAll (k l m n))))
                                                   else fm ts
                      fl tss = case tss of
                        [] -> fm (findSubterms m)
                        (Exs1 ex tx : ts) -> case getPrfHasSin tx of
                                PrfHasSin -> if hasSubterm ex m || hasSubterm ex n
                                             then chg (LeT ex (absSubterm (Var Zro) (sucAll ex) (sucAll (k l m n))))
                                             else fl ts
                  in fl (findSubterms l)

cseOne2 :: (TG.Type a, TG.Type b, TG.Type c) =>
           (Exp s g a -> Exp s g b -> Exp s g c) ->
            Exp s g a -> Exp s g b -> Chg (Exp s g c)
cseOne2 k m n = let f tss = case tss of
                       [] ->  k <$> cseOne m <*> cseOne n
                       (Exs1 ex tx : ts) -> case getPrfHasSin tx of
                                     PrfHasSin -> if hasSubterm ex n
                                                  then chg (LeT ex (absSubterm (Var Zro) (sucAll ex) (sucAll (k m n))))
                                                  else f ts
                in  f (findSubterms m)


cseOne2F :: (TG.Type a, TG.Type b, TG.Type c) =>
           (Exp s g a -> Exp s (a ': g) b -> Exp s g c) ->
            Exp s g a -> Exp s (a ': g) b -> Chg (Exp s g c)
cseOne2F k m n = let f tss = case tss of
                       [] ->  k <$> cseOne m <*> cseOne n
                       (Exs1 ex tx : ts) -> case getPrfHasSin tx of
                                     PrfHasSin -> if hasSubterm (sucAll ex) n
                                                  then chg (LeT ex (absSubterm (Var Zro) (sucAll ex) (sucAll (k m n))))
                                                  else f ts
                 in  f (findSubterms m)


absSubterm :: forall a b s g.
          (TG.Type a , TG.Type b) =>
          Exp s g b -> Exp s g b -> Exp s g a -> Exp s g a
absSubterm xx ex ee = let t = sin :: TG.Typ a in
  case eqlSin (sinTyp ex) t of
    Rgt Rfl | eql ex ee -> xx
    _  -> case ee of
      Prm x es -> Prm x (TG.mapC (absSubterm xx ex) es)
      _        -> $(genOverloadedW 'ee ''Exp  ['Prm] (trvWrp 't)
        (\ tt -> if
           | matchQ tt [t| Exp a (a ': a) a |]     -> [| absSubterm (sucAll xx) (sucAll ex) |]
           | matchQ tt [t| Exp a a a |]            -> [| absSubterm xx ex |]
           | otherwise                             -> [| id |]))


hasSubterm :: (TG.Type b , TG.Type a) => Exp s g b -> Exp s g a -> Bool
hasSubterm ex ee = numSubterm ex ee /= (0 :: Word32)

numSubterm :: forall s g a b . (TG.Type a , TG.Type b) =>
              Exp s g b -> Exp s g a -> Word32
numSubterm ex ee = let t = sin :: TG.Typ a in
                   (case eqlSin (sinTyp ex) t of
                       Rgt Rfl  -> if eql ex ee
                                   then (1 :: Word32)
                                   else 0
                       _        -> 0) + (case ee of
    Prm _ es -> TG.fld (\ b e -> b + numSubterm ex e) 0 es
    _        -> $(recAppMQ 'ee ''Exp (const [| 0 :: Word32 |]) ['Prm]
                            [| \ _x -> (0 :: Word32) |] [| (+) |] [| (+) |] (trvWrp 't)
                  (\ tt -> if
                       | matchQ tt [t| Exp a (a ': a) a |]     -> [| numSubterm (sucAll ex) |]
                       | matchQ tt [t| Exp a a a |]            -> [| numSubterm ex |]
                       | otherwise                             -> [| const (0 :: Word32)  |])))

findSubterms :: forall s g a. TG.Type a =>
                Exp s g a -> [Exs1 (Exp s g) TG.Typ]
findSubterms ee = let t = sin :: TG.Typ a in
                  (if not (isVal ee) && not (partialApp ee)
                   then ((Exs1 ee (sinTyp ee)) :)
                   else id)(case ee of
    Prm _ es -> TG.fld (\ b e -> b ++ findSubterms e) [] es
    _        -> $(recAppMQ 'ee ''Exp (const [| [] |]) ['Prm]
                               [| \ _x -> [] |] [| (++) |] [| (++) |] (trvWrp 't)
                  (\ tt -> if
                       | matchQ tt [t| Exp a (a ': a) a |]     -> [| findSubtermsF |]
                       | matchQ tt [t| Exp a a a |]            -> [| findSubterms |]
                       | otherwise                             -> [| const [] |])))

findSubtermsF :: TG.Type b => Exp s (a ': g) b -> [Exs1 (Exp s g) TG.Typ]
findSubtermsF e = foldr (\ (Exs1 eg t) xxs -> case prdAllM eg of
                                                Nothing  ->  xxs
                                                Just eg' -> (Exs1 eg' t) : xxs) [] (findSubterms e)

partialApp :: TG.Type a => Exp s g a -> Bool
partialApp (App l _) = case sinTyp l of
  TG.Arr _ (TG.Arr _ _) -> True
  _                       -> False
partialApp _         = False
