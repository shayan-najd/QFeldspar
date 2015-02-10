{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-max-relevant-binds #-}
module QFeldspar.CSE.MiniFeldspar where

import QFeldspar.Expression.MiniFeldspar
import QFeldspar.Expression.Utils.MiniFeldspar(absTmp,eql)
import QFeldspar.Expression.Utils.Common

import QFeldspar.MyPrelude hiding (foldl)

import qualified QFeldspar.Type.GADT as TFG
import qualified QFeldspar.Environment.Typed  as ET
import QFeldspar.ChangeMonad
import QFeldspar.Singleton
import QFeldspar.Variable.Typed

import Data.Constraint
import Data.Constraint.Unsafe

cse :: HasSin TFG.Typ t => Exp r t -> Exp r t
cse = tilNotChg cseOne

cseF :: forall r a b. (HasSin TFG.Typ a , HasSin TFG.Typ b) =>
           (Exp r a -> Exp r b) -> (Exp r a -> Exp r b)
cseF f  = let v = genNewNam "cseF"
              {-# NOINLINE v #-}
          in deepseq v $ (\ x -> absTmp x v (cse (f (Tmp v))))

hasTagEnv :: HasSin TFG.Typ t => String -> Exp r t -> TFG.Typ z -> ET.Env (Exp r) (TFG.Arg z) -> Bool
hasTagEnv x ex = TFG.fld (\ b e -> b || hasTag x ex e) False

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

cseOneEnv :: forall t ra rb r.
       (HasSin TFG.Typ t , TFG.Arg t ~ Add ra rb) =>
       Var r t -> ET.Env (Exp r) ra -> ET.Env (Exp r) rb ->
                  Chg (Exp r (TFG.Out t))
cseOneEnv v r r' = case r' of
  ET.Emp           -> AppV v <$> (TFG.mapMC (sinTyp v) cseOne (ET.add r r'))
  ET.Ext (e :: Exp r tx) (es :: ET.Env (Exp r) txs) ->
    case TFG.getPrf (sinTyp v) (ET.fmap (\ _ -> T) r) (ET.fmap (\ _ -> T) r') of
      PrfHasSin   -> case unsafeCoerceConstraint
           :: () :- (Add (Add ra (tx ': '[])) txs ~ Add ra (tx ': txs)) of
       Sub Dict  -> case findTag e of
        Just(x , Exs1 ex tx) -> case getPrfHasSin tx of
                   PrfHasSin -> case TFG.getTypTailEnv (sinTyp v) (ET.add r (ET.Ext e ET.Emp)) es of
                     ExsSin ttx -> case TFG.eqlArg (TFG.getSinDiff (sinTyp v)
                                                   (ET.add r (ET.Ext e ET.Emp)) es) ttx of
                       Rgt TFG.EqlArg -> if hasTagEnv x ex ttx es then
                                             chg (Let ex (\ xx ->
                                                              AppV v (TFG.mapC (sinTyp v) (absTag xx x ex) (ET.add r r'))))
                                         else if (numTag x ex e == 1) then
                                             chg (remTheTag x (AppV v (ET.add r r')))
                                         else
                                             cseOneEnv v (ET.add r (ET.Ext e ET.Emp)) es
        _  -> cseOneEnv v (ET.add r (ET.Ext e ET.Emp)) es

cseOne3 :: (NumTag a, NumTag b, NumTag c, FindTag a, FindTag b, CSEOne a,
            CSEOne b, CSEOne c, HasSin TFG.Typ t, Env a ~ Env b, Env b ~ Env c) =>
           (a -> b -> c -> Exp (Env b) t) ->
           a -> b -> c -> Chg (Exp (Env b) t)
cseOne3 k l m n  = case findTag l of
    Just(x , Exs1 ex tx) -> case getPrfHasSin tx of
               PrfHasSin -> if hasTag x ex m || hasTag x ex n then
                                chg (Let ex (\ xx -> absTag xx x ex (k l m n)))
                            else
                                k <$> cseOne l <*> cseOne m <*> cseOne n
    _ -> case findTag m of
     Just(x , Exs1 ex tx) -> case getPrfHasSin tx of
                PrfHasSin -> if hasTag x ex n then
                                 chg (Let ex (\ xx -> absTag xx x ex (k l m n)))
                             else
                                 k <$> cseOne l <*> cseOne m <*> cseOne n
     _ -> k <$> cseOne l <*> cseOne m <*> cseOne n

cseOne2 :: (FindTag a, NumTag a,NumTag b,
            CSEOne a, CSEOne b,
            HasSin TFG.Typ c,Env b ~ Env a) =>
           (a -> b -> Exp (Env a) c) ->
            a -> b -> Chg (Exp (Env a) c)
cseOne2 k m n = case findTag m of
      Just(x , Exs1 ex tx) -> case getPrfHasSin tx of
                 PrfHasSin -> if hasTag x ex n then
                                  chg (Let ex (\ xx -> absTag xx x ex (k m n)))
                              else
                                  k <$> cseOne m <*> cseOne n
      _ -> k <$> cseOne m <*> cseOne n

class CSEOne a where
  cseOne :: a -> Chg a

instance HasSin TFG.Typ t => CSEOne (Exp r t) where
 cseOne ee = let t = sin :: TFG.Typ t in case ee of
  ConI i                    -> pure (ConI i)
  ConB b                    -> pure (ConB b)
  ConF f                    -> pure (ConF f)
  AppV v es                 -> cseOneEnv v ET.Emp es
  Cnd ec et ef              -> cseOne3 Cnd ec et ef
  Whl ec eb ei              -> cseOne3 Whl ec eb ei
  Tpl ef es                 -> case TFG.getPrfHasSinTpl t of
    (PrfHasSin , PrfHasSin) -> cseOne2 Tpl ef es
  Fst e                     -> Fst <$> cseOne e
  Snd e                     -> Snd <$> cseOne e
  Ary el ef                 -> case TFG.getPrfHasSinAry t of
    PrfHasSin               -> cseOne2 Ary el ef
  Len e                     -> Len <$> cseOne e
  Ind ea ei                 -> cseOne2 Ind ea ei
  Let el eb                 -> cseOne2 Let el eb
  Cmx er ei                 -> cseOne2 Cmx er ei
  Tmp x                     -> pure (Tmp x)
  Tag x e                   -> Tag x <$> cseOne e
  Mul er ei                 -> cseOne2 Mul er ei

instance (HasSin TFG.Typ a , HasSin TFG.Typ b) => CSEOne (Exp r a -> Exp r b) where
 cseOne f  = let v = genNewNam "cseOneF"
                 {-# NOINLINE v #-}
             in deepseq v $ do eb <- cseOne (f (Tmp v))
                               return (\ x -> absTmp x v eb)

infixl 4 <||>

(<||>) :: Maybe a -> Maybe a -> Maybe a
Nothing  <||> m = m
(Just x) <||> _ = Just x

class FindTag a where
 type Env a :: [*]
 findTag :: a -> Maybe (String , Exs1 (Exp (Env a)) TFG.Typ)

instance HasSin TFG.Typ t => FindTag (Exp r t) where
 type Env (Exp r t) = r
 findTag ee = let t = sin :: TFG.Typ t in case ee of
  AppV v es -> TFG.fld (\ b e -> (findTag e) <||> b) Nothing
               (sinTyp v) es
  Tag x e   -> Just (x , Exs1 e (sinTyp e))
  _         -> $(recAppMQ 'ee ''Exp (const [| Nothing |]) ['AppV,'Tag]
    [| \ _x -> Nothing |] [| (<||>) |] [| (<||>) |] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| Exp t t -> Exp t t |] -> [| findTag |]
    | matchQ tt [t| Exp t t |]            -> [| findTag |]
    | otherwise                           -> [| const Nothing |]))

instance (HasSin TFG.Typ a , HasSin TFG.Typ b) =>
         FindTag (Exp r a -> Exp r b) where
 type Env (Exp r a -> Exp r b) = r
 findTag f = let v = genNewNam "findTagF"
                 {-# NOINLINE v #-}
             in  deepseq v $ findTag (f (Tmp v))

class AbsTag a where
 absTag :: HasSin TFG.Typ t' =>
           Exp (Env a) t' -> String -> Exp (Env a) t' ->a -> a

instance HasSin TFG.Typ t => AbsTag (Exp r t) where
 absTag xx s ex ee = let t = sin :: TFG.Typ t in case ee of
  AppV v es    -> AppV v (TFG.mapC (sinTyp v) (absTag xx s ex) es)
  Tag x e
   | s == x    -> case eqlSin (sinTyp xx) t of
     Rgt Rfl   -> if eql e ex
                  then xx
                  else Tag x (absTag xx s ex e)
     _         -> impossible
   | otherwise -> Tag x (absTag xx s ex e)
  _            -> $(genOverloadedW 'ee ''Exp  ['AppV,'Tag] (trvWrp 't)
   (\ tt -> if
     | matchQ tt [t| Exp t t -> Exp t t |] -> [| absTag xx s ex |]
     | matchQ tt [t| Exp t t |]            -> [| absTag xx s ex |]
     | otherwise                           -> [| id |]))

instance HasSin TFG.Typ b => AbsTag (Exp g a -> Exp g b) where
 absTag xx s ex = (absTag xx s ex .)

hasTag :: (NumTag a , HasSin TFG.Typ t) => String -> Exp (Env a) t -> a -> Bool
hasTag s ex ee = numTag s ex ee /= 0

class NumTag a where
    numTag :: HasSin TFG.Typ t => String -> Exp (Env a) t -> a -> Int

instance HasSin TFG.Typ a => NumTag (Exp g a) where
 numTag s ex ee = let t = sin :: TFG.Typ a in case ee of
  AppV v es    -> TFG.fld (\ b e -> b + numTag s ex e) 0 (sinTyp v) es
  Tag x e
   | s == x    -> case eqlSin (sinTyp ex) t of
      Rgt Rfl  -> if eql ex e
                  then 1
                  else numTag s ex e
   | otherwise -> numTag s ex e
  _            -> $(recAppMQ 'ee ''Exp (const [| 0 |]) ['AppV,'Tag]
     [| \ _x -> 0 |] [| (+) |] [| (+) |] (trvWrp 't)
    (\ tt -> if
     | matchQ tt [t| Exp a a -> Exp a a |] -> [| numTag s ex |]
     | matchQ tt [t| Exp a a |]            -> [| numTag s ex |]
     | otherwise                           -> [| const 0     |]))

instance (HasSin TFG.Typ a , HasSin TFG.Typ b) =>
         NumTag (Exp g a -> Exp g b) where
 numTag s ex f = let v = genNewNam "numTagF"
                     {-# NOINLINE v #-}
                 in deepseq v $ numTag s ex (f (Tmp v))
