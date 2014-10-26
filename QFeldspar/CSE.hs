module QFeldspar.CSE where

import Prelude (($))
import QFeldspar.Expression.Feldspar.MiniWellScoped

import QFeldspar.MyPrelude hiding (foldl)

import qualified QFeldspar.Type.Feldspar.GADT as TFG
import qualified QFeldspar.Environment.Typed  as ET
import QFeldspar.ChangeMonad
import QFeldspar.Singleton
import QFeldspar.Variable.Typed

import Data.Constraint
import Data.Constraint.Unsafe

import Debug.Trace

cse :: forall r t. HasSin TFG.Typ t => Exp r t -> Exp r t
cse e = remTag (tilNotChg cseOne e)

cseF :: forall r a b. (HasSin TFG.Typ a , HasSin TFG.Typ b) =>
           (Exp r a -> Exp r b) -> (Exp r a -> Exp r b)
cseF f  = let v = genNewNam "cseF"
              {-# NOINLINE v #-}
          in deepseq v $ remTag . (\ x -> absTmp x v (cse (f (Tmp v))))

hasTagEnv :: String -> ET.Env (Exp r) r' -> Bool
hasTagEnv x es = ET.foldl (\ b e -> b || hasTag x e) False es

remTag :: Exp r t -> Exp r t
remTag ee = case ee of
  ConI i                    -> ConI i
  ConB i                    -> ConB i
  ConF i                    -> ConF i
  AppV v es                 -> AppV v (TFG.mapC (sinTyp v) remTag es)
  Cnd ec et ef              -> Cnd (remTag ec) (remTag et) (remTag ef)
  Whl ec eb ei              -> Whl (remTag . ec) (remTag . eb) (remTag ei)
  Tpl ef es                 -> Tpl (remTag ef)   (remTag es)
  Fst e                     -> Fst (remTag e)
  Snd e                     -> Snd (remTag e)
  Ary el ef                 -> Ary (remTag el)   (remTag . ef)
  Len e                     -> Len (remTag e)
  Ind ea ei                 -> Ind (remTag ea)   (remTag ei)
  Let el eb                 -> Let (remTag el)   (remTag . eb)
  Cmx er ei                 -> Cmx (remTag er)   (remTag ei)
  Tmp x                     -> Tmp x
  Tag _ e                   -> remTag e
  Non                       -> Non
  Som e                     -> Som (remTag e)
  May em en es              -> May (remTag em) (remTag en) (remTag . es)

remTheTag :: String -> Exp r t -> Exp r t
remTheTag x ee = case ee of
  ConI i                    -> ConI i
  ConB i                    -> ConB i
  ConF i                    -> ConF i
  AppV v es                 -> AppV v (TFG.mapC (sinTyp v) (remTheTag x) es)
  Cnd ec et ef              -> Cnd (remTheTag x ec) (remTheTag x et) (remTheTag x ef)
  Whl ec eb ei              -> Whl (remTheTag x . ec) (remTheTag x . eb) (remTheTag x ei)
  Tpl ef es                 -> Tpl (remTheTag x ef)   (remTheTag x es)
  Fst e                     -> Fst (remTheTag x e)
  Snd e                     -> Snd (remTheTag x e)
  Ary el ef                 -> Ary (remTheTag x el)   (remTheTag x . ef)
  Len e                     -> Len (remTheTag x e)
  Ind ea ei                 -> Ind (remTheTag x ea)   (remTheTag x ei)
  Let el eb                 -> Let (remTheTag x el)   (remTheTag x . eb)
  Cmx er ei                 -> Cmx (remTheTag x er)   (remTheTag x ei)
  Tmp n                     -> Tmp n
  Tag y e
      | x == y              -> e
      | otherwise           -> Tag y (remTheTag x e)
  Non                       -> Non
  Som e                     -> Som (remTheTag x e)
  May em en es              -> May (remTheTag x em) (remTheTag x en) (remTheTag x . es)

cmt :: forall t ra rb r.
       (HasSin TFG.Typ t , TFG.Arg t ~ Add ra rb) =>
       Var r t -> ET.Env (Exp r) ra -> ET.Env (Exp r) rb ->
                  Chg (Exp r (TFG.Out t))
cmt v r r' = case r' of
  ET.Emp           -> AppV v <$> (TFG.mapMC (sinTyp v) cseOne (ET.add r r'))
  ET.Ext (e :: Exp r tx) (es :: ET.Env (Exp r) txs) ->
    case TFG.getPrf (sinTyp v) (ET.fmap (\ _ -> T) r) (ET.fmap (\ _ -> T) r') of
      PrfHasSin   -> case unsafeCoerceConstraint
           :: () :- (Add (Add ra (tx ': '[])) txs ~ Add ra (tx ': txs)) of
       Sub Dict  -> case findTag e of
        Just(~x , Exs1 ex tx)
            | hasTagEnv x es -> case getPrfHasSin tx of
               PrfHasSin -> chg (Let ex (\ xx ->
                 AppV v (TFG.mapC (sinTyp v) (absTag xx x) (ET.add r r'))))
            | numTag x e == 1 -> chg (remTheTag x (AppV v (ET.add r r')))
        _  ->  cmt v (ET.add r (ET.Ext e ET.Emp)) es


cseOne :: forall r t. HasSin TFG.Typ t => Exp r t -> Chg (Exp r t)
cseOne ee = let t = sin :: TFG.Typ t in case ee of
  ConI i                    -> pure (ConI i)
  ConB b                    -> pure (ConB b)
  ConF f                    -> pure (ConF f)
  AppV v es                 -> cmt v ET.Emp es
  Cnd ec et ef              -> case findTag ec of
    Just(~x , Exs1 ex tx)
        | hasTag x et || hasTag x ef -> case getPrfHasSin tx of
           PrfHasSin -> chg (Let ex (\ xx ->
             Cnd (absTag xx x ec) (absTag xx x et) (absTag xx x ef)))
        | numTag x ec == 1 -> chg (remTheTag x ee)
    _ -> case findTag et of
     Just(~x , Exs1 ex tx)
        | hasTag x ef -> case getPrfHasSin tx of
            PrfHasSin -> chg (Let ex (\ xx ->
              Cnd (absTag xx x ec) (absTag xx x et) (absTag xx x ef)))
        | numTag x et == 1 -> chg (remTheTag x ee)
     _ -> Cnd <$> cseOne ec <*> cseOne et <*> cseOne ef
  Whl ec eb ei              -> case findTagF ec of
    Just(~x , Exs1 ex tx)
        | hasTagF x eb || hasTag x ei -> case getPrfHasSin tx of
           PrfHasSin -> chg (Let ex (\ xx ->
             Whl (absTag xx x . ec) (absTag xx x . eb) (absTag xx x ei)))
        | numTagF x ec == 1 -> chg (remTheTag x ee)
    _ -> case findTagF eb of
     Just(~x , Exs1 ex tx)
        | hasTag x ei -> case getPrfHasSin tx of
            PrfHasSin -> chg (Let ex (\ xx ->
              Whl (absTag xx x . ec) (absTag xx x . eb) (absTag xx x ei)))
        | numTagF x eb == 1 -> chg (remTheTag x ee)
     _ -> Whl <$> cseOneF ec <*> cseOneF eb <*> cseOne ei
  Tpl ef es                 -> case TFG.getPrfHasSinTpl t of
    (PrfHasSin , PrfHasSin) -> case findTag ef of
      Just(~x , Exs1 ex tx)
        | hasTag x es -> case getPrfHasSin tx of
            PrfHasSin -> chg (Let ex (\ xx ->
              Tpl (absTag xx x ef) (absTag xx x es)))
        | numTag x ef == 1 -> chg (remTheTag x ee)
      _ -> Tpl <$> cseOne ef <*> cseOne es
  Fst e                     -> Fst <$> cseOne e
  Snd e                     -> Snd <$> cseOne e
  Ary el ef                 -> case TFG.getPrfHasSinAry t of
    PrfHasSin               -> case findTag el of
      Just(~x , Exs1 ex tx)
        | hasTagF x ef -> case getPrfHasSin tx of
            PrfHasSin -> chg (Let ex (\ xx ->
              Ary (absTag xx x el) (absTag xx x . ef)))
        | numTag x el == 1 -> chg (remTheTag x ee)
      _ -> Ary <$> cseOne el <*> cseOneF ef
  Len e                     -> Len <$> cseOne e
  Ind ea ei                 -> case findTag ea of
      Just(~x , Exs1 ex tx)
        | hasTag x ei -> case getPrfHasSin tx of
            PrfHasSin -> chg (Let ex (\ xx ->
              Ind (absTag xx x ea) (absTag xx x ei)))
        | numTag x ea == 1 -> chg (remTheTag x ee)
      _ -> Ind <$> cseOne ea <*> cseOne ei
  Let el eb                 -> case findTag el of
      Just(~x , Exs1 ex tx)
        | hasTagF x eb -> case getPrfHasSin tx of
            PrfHasSin -> chg (Let ex (\ xx ->
              Let (absTag xx x el) (absTag xx x . eb)))
        | numTag x el == 1 -> chg (remTheTag x ee)
      _ -> Let <$> cseOne el <*> cseOneF eb
  Cmx er ei                 -> case findTag er of
      Just(~x , Exs1 ex tx)
        | hasTag x ei -> case getPrfHasSin tx of
            PrfHasSin -> chg (Let ex (\ xx ->
              Cmx (absTag xx x er) (absTag xx x ei)))
        | numTag x er == 1 -> chg (remTheTag x ee)
      _ -> Cmx <$> cseOne er <*> cseOne ei
  Tmp x                     -> pure (Tmp x)
  Tag x e                   -> Tag x <$> cseOne e
  Non                       -> pure Non
  Som e                     -> case TFG.getPrfHasSinMay t of
    PrfHasSin               -> Som <$> cseOne e
  May em en es              -> case findTag em of
    Just(~x , Exs1 ex tx)
        | hasTag x en|| hasTagF x es -> case getPrfHasSin tx of
            PrfHasSin -> chg (Let ex (\ xx ->
              May (absTag xx x em) (absTag xx x  en) (absTag xx x . es)))
        | numTag x em == 1 -> chg (remTheTag x ee)
    _ -> case findTag en of
     Just(~x , Exs1 ex tx)
        | hasTagF x es -> case getPrfHasSin tx of
            PrfHasSin -> chg (Let ex (\ xx ->
              May (absTag xx x em) (absTag xx x  en) (absTag xx x . es)))
        | numTag x en == 1 -> chg (remTheTag x ee)
     _ -> May <$> cseOne em <*> cseOne en <*> cseOneF es

cseOneF :: forall r a b. (HasSin TFG.Typ a , HasSin TFG.Typ b) =>
           (Exp r a -> Exp r b) -> Chg (Exp r a -> Exp r b)
cseOneF f  = let v = genNewNam "cseOneF"
                 {-# NOINLINE v #-}
             in deepseq v $ do eb <- cseOne (f (Tmp v))
                               return (\ x -> absTmp x v eb)

infixl 4 <||>

(<||>) :: Maybe a -> Maybe a -> Maybe a
Nothing  <||> m = m
(Just x) <||> _ = Just x

findTag :: forall r t. HasSin TFG.Typ t =>
          Exp r t -> Maybe (String , Exs1 (Exp r) TFG.Typ)
findTag ee = let t = sin :: TFG.Typ t in case ee of
  ConI _                    -> Nothing
  ConB _                    -> Nothing
  ConF _                    -> Nothing
  AppV v es                 -> TFG.fld (\ b e -> b <||> (findTag e)) Nothing
                               (sinTyp v) es
  Cnd ec et ef              -> findTag  ec <||> findTag  et <||> findTag ef
  Whl ec eb ei              -> findTagF ec <||> findTagF eb <||> findTag ei
  Tpl ef es                 -> case TFG.getPrfHasSinTpl t of
    (PrfHasSin , PrfHasSin) -> findTag  ef <||> findTag  es
  Fst e                     -> findTag  e
  Snd e                     -> findTag  e
  Ary el ef                 -> case TFG.getPrfHasSinAry t of
    PrfHasSin               -> findTag  el <||> findTagF ef
  Len e                     -> findTag  e
  Ind ea ei                 -> findTag  ea <||> findTag  ei
  Let el eb                 -> findTag  el <||> findTagF eb
  Cmx er ei                 -> findTag  er <||> findTag  ei
  Tmp _                     -> Nothing
  Tag x e                   -> Just (x , Exs1 e (sinTyp e))
  Non                       -> Nothing
  Som e                     -> case TFG.getPrfHasSinMay t of
    PrfHasSin               -> findTag e
  May em en es              -> findTag em <||> findTag en <||> findTagF es

findTagF :: (HasSin TFG.Typ a , HasSin TFG.Typ b) =>
            (Exp r a -> Exp r b) -> Maybe (String , Exs1 (Exp r) TFG.Typ)
findTagF f = let v = genNewNam "findTagF"
                 {-# NOINLINE v #-}
             in  deepseq v $ findTag (f (Tmp v))


absTag :: forall r t t'. (HasSin TFG.Typ t', HasSin TFG.Typ t) =>
          Exp r t' -> String -> Exp r t -> Exp r t
absTag xx s ee = let t = sin :: TFG.Typ t in case ee of
  ConI i                    -> ConI i
  ConB i                    -> ConB i
  ConF i                    -> ConF i
  AppV v es                 -> AppV v (TFG.mapC (sinTyp v) (absTag xx s) es)
  Cnd ec et ef              -> Cnd (absTag xx s ec)   (absTag xx s et)
                                    (absTag xx s ef)
  Whl ec eb ei              -> Whl (absTag xx s . ec) (absTag xx s . eb)
                                   (absTag xx s ei)
  Tpl ef es                 -> case TFG.getPrfHasSinTpl t of
    (PrfHasSin , PrfHasSin) -> Tpl (absTag xx s ef)   (absTag xx s es)
  Fst e                     -> Fst (absTag xx s e)
  Snd e                     -> Snd (absTag xx s e)
  Ary el ef                 -> case TFG.getPrfHasSinAry t of
    PrfHasSin               -> Ary (absTag xx s el)   (absTag xx s . ef)
  Len e                     -> Len (absTag xx s e)
  Ind ea ei                 -> Ind (absTag xx s ea)   (absTag xx s ei)
  Let el eb                 -> Let (absTag xx s el)   (absTag xx s . eb)
  Cmx er ei                 -> Cmx (absTag xx s er)   (absTag xx s ei)
  Tmp x                     -> Tmp x
  Tag x e
    | s == x                -> case eqlSin (sinTyp xx) t of
      Rgt Rfl               -> xx
      _                     -> impossible
    | otherwise             -> Tag x (absTag xx s e)
  Non                       -> Non
  Som e                     -> case TFG.getPrfHasSinMay t of
    PrfHasSin               -> Som (absTag xx s e)
  May ec eb ei              -> May (absTag xx s ec) (absTag xx s eb)
                                   (absTag xx s . ei)


hasTag :: String -> Exp r t -> Bool
hasTag s ee = case ee of
  ConI _                    -> False
  ConB _                    -> False
  ConF _                    -> False
  AppV _ es                 -> ET.foldl (\ b e -> b || hasTag s e) False es
  Cnd ec et ef              -> hasTag s ec || hasTag s et || hasTag s ef
  Whl ec eb ei              -> hasTagF s ec || hasTagF s eb || hasTag s ei
  Tpl ef es                 -> hasTag s ef || hasTag s es
  Fst e                     -> hasTag s e
  Snd e                     -> hasTag s e
  Ary el ef                 -> hasTag s el || hasTagF s ef
  Len e                     -> hasTag s e
  Ind ea ei                 -> hasTag s ea || hasTag s ei
  Let el eb                 -> hasTag s el || hasTagF s eb
  Cmx er ei                 -> hasTag s er || hasTag s ei
  Tmp _                     -> False
  Tag x e
    | s == x                -> True
    | otherwise             -> hasTag s e
  Non                       -> False
  Som e                     -> hasTag s e
  May ec eb ei              -> hasTag s ec || hasTag s eb || hasTagF s ei

hasTagF :: String -> (Exp r ta -> Exp r tb) -> Bool
hasTagF s f = let v = genNewNam "hasTagF"
                  {-# NOINLINE v #-}
              in deepseq v $ hasTag s (f (Tmp v))


numTag :: String -> Exp r t -> Int
numTag s ee = case ee of
  ConI _                    -> 0
  ConB _                    -> 0
  ConF _                    -> 0
  AppV _ es                 -> ET.foldl (\ b e -> b + numTag s e) 0 es
  Cnd ec et ef              -> numTag s ec + numTag s et + numTag s ef
  Whl ec eb ei              -> numTagF s ec + numTagF s eb + numTag s ei
  Tpl ef es                 -> numTag s ef + numTag s es
  Fst e                     -> numTag s e
  Snd e                     -> numTag s e
  Ary el ef                 -> numTag s el + numTagF s ef
  Len e                     -> numTag s e
  Ind ea ei                 -> numTag s ea + numTag s ei
  Let el eb                 -> numTag s el + numTagF s eb
  Cmx er ei                 -> numTag s er + numTag s ei
  Tmp _                     -> 0
  Tag x e
    | s == x                -> 1
    | otherwise             -> numTag s e
  Non                       -> 0
  Som e                     -> numTag s e
  May ec eb ei              -> numTag s ec + numTag s eb + numTagF s ei

numTagF :: String -> (Exp r ta -> Exp r tb) -> Int
numTagF s f = let v = genNewNam "numTagF"
                  {-# NOINLINE v #-}
              in deepseq v $ numTag s (f (Tmp v))
