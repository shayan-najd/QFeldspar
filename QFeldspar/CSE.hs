{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.CSE where

import QFeldspar.Expression.Feldspar.MiniFeldspar
import QFeldspar.Expression.Feldspar.Utils.MiniFeldspar(absTmp)
import QFeldspar.Expression.Feldspar.Utils.Common

import QFeldspar.MyPrelude hiding (foldl)

import qualified QFeldspar.Type.Feldspar.GADT as TFG
import qualified QFeldspar.Environment.Typed  as ET
import QFeldspar.ChangeMonad
import QFeldspar.Singleton
import QFeldspar.Variable.Typed

import Data.Constraint
import Data.Constraint.Unsafe

cse :: forall r t. HasSin TFG.Typ t => Exp r t -> Exp r t
cse e = tilNotChg cseOne e

cseF :: forall r a b. (HasSin TFG.Typ a , HasSin TFG.Typ b) =>
           (Exp r a -> Exp r b) -> (Exp r a -> Exp r b)
cseF f  = let v = genNewNam "cseF"
              {-# NOINLINE v #-}
          in deepseq v $ (\ x -> absTmp x v (cse (f (Tmp v))))

hasTagEnv :: String -> ET.Env (Exp r) r' -> Bool
hasTagEnv x es = ET.foldl (\ b e -> b || hasTag x e) False es

remTag :: forall r t. Exp r t -> Exp r t
remTag ee = case ee of
  AppV v es -> AppV v (TFG.mapC (sinTyp v) remTag es)
  Tag _  e  -> remTag e
  _         -> $(genOverloaded 'ee ''Exp  ['AppV,'Tag]
   (\ tt -> if
    | matchQ tt [t| Exp t t -> Exp t t |] -> [| remTagF |]
    | matchQ tt [t| Exp t t |]            -> [| remTag  |]
    | otherwise                           -> [| id |]))

remTagF :: (Exp r a -> Exp r b) -> (Exp r a -> Exp r b)
remTagF = (remTag .)

remTheTag :: forall r t. String -> Exp r t -> Exp r t
remTheTag x ee = case ee of
  AppV v es       -> AppV v (TFG.mapC (sinTyp v) (remTheTag x) es)
  Tag y e
      | x == y    -> e
      | otherwise -> Tag y (remTheTag x e)
  _               -> $(genOverloaded 'ee ''Exp  ['AppV,'Tag]
   (\ tt -> if
    | matchQ tt [t| Exp t t -> Exp t t |] -> [| (remTheTag x .) |]
    | matchQ tt [t| Exp t t |]            -> [| remTheTag  x |]
    | otherwise                           -> [| id |]))

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
  Mul er ei                 -> case findTag er of
      Just(~x , Exs1 ex tx)
        | hasTag x ei -> case getPrfHasSin tx of
            PrfHasSin -> chg (Let ex (\ xx ->
              Mul (absTag xx x er) (absTag xx x ei)))
        | numTag x er == 1 -> chg (remTheTag x ee)
      _ -> Mul <$> cseOne er <*> cseOne ei

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
  AppV v es -> TFG.fld (\ b e -> (findTag e) <||> b) Nothing
               (sinTyp v) es
  Tag x e   -> Just (x , Exs1 e (sinTyp e))
  _         -> $(recAppMQ 'ee ''Exp (const [| Nothing |]) ['AppV,'Tag]
    [| \ _x -> Nothing |] [| (<||>) |] [| (<||>) |] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| Exp t t -> Exp t t |] -> [| findTagF |]
    | matchQ tt [t| Exp t t |]            -> [| findTag  |]
    | otherwise                           -> [| const Nothing |]))

findTagF :: (HasSin TFG.Typ a , HasSin TFG.Typ b) =>
            (Exp r a -> Exp r b) -> Maybe (String , Exs1 (Exp r) TFG.Typ)
findTagF f = let v = genNewNam "findTagF"
                 {-# NOINLINE v #-}
             in  deepseq v $ findTag (f (Tmp v))

absTag :: forall r t t'. (HasSin TFG.Typ t', HasSin TFG.Typ t) =>
          Exp r t' -> String -> Exp r t -> Exp r t
absTag xx s ee = let t = sin :: TFG.Typ t in case ee of
 AppV v es    -> AppV v (TFG.mapC (sinTyp v) (absTag xx s) es)
 Tag x e
  | s == x    -> case eqlSin (sinTyp xx) t of
    Rgt Rfl   -> xx
    _         -> impossible
  | otherwise -> Tag x (absTag xx s e)
 _            -> $(genOverloadedW 'ee ''Exp  ['AppV,'Tag] (trvWrp 't)
  (\ tt -> if
    | matchQ tt [t| Exp t t -> Exp t t |] -> [| (absTag xx s .) |]
    | matchQ tt [t| Exp t t |]            -> [| absTag xx s |]
    | otherwise                           -> [| id |]))

hasTag :: String -> Exp r t -> Bool
hasTag s ee = numTag s ee /= 0

hasTagF :: String -> (Exp r ta -> Exp r tb) -> Bool
hasTagF s f = let v = genNewNam "hasTagF"
                  {-# NOINLINE v #-}
              in deepseq v $ hasTag s (f (Tmp v))

numTag :: forall r t. String -> Exp r t -> Int
numTag s ee = case ee of
 AppV _ es    -> ET.foldl (\ b e -> b + numTag s e) 0 es
 Tag x e
  | s == x    -> 1
  | otherwise -> numTag s e
 _            -> $(recAppMQ 'ee ''Exp (const [| 0 |]) ['AppV,'Tag]
    [| \ _x -> 0 |] [| (+) |] [| (+) |] (const id)
   (\ tt -> if
    | matchQ tt [t| Exp t t -> Exp t t |] -> [| numTagF s |]
    | matchQ tt [t| Exp t t |]            -> [| numTag  s |]
    | otherwise                           -> [| const 0   |]))

numTagF :: forall r ta tb.
           String -> (Exp r ta -> Exp r tb) -> Int
numTagF s f = let v = genNewNam "numTagF"
                  {-# NOINLINE v #-}
              in deepseq v $ numTag s (f (Tmp v))
