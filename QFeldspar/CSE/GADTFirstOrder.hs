{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-max-relevant-binds #-}
module QFeldspar.CSE.GADTFirstOrder where

import QFeldspar.Expression.GADTFirstOrder
import QFeldspar.Expression.Utils.GADTFirstOrder(eql,prdAllM,sucAll)
import QFeldspar.Expression.Utils.Common

import QFeldspar.MyPrelude hiding (foldl)

import qualified QFeldspar.Type.GADT as TFG
import QFeldspar.ChangeMonad
import QFeldspar.Singleton
import QFeldspar.Variable.Typed

cse :: HasSin TFG.Typ t => Exp r t -> Exp r t
cse = tilNotChg cseOne

cseOne3 :: (HasSin TFG.Typ t , HasSin TFG.Typ a , HasSin TFG.Typ b , HasSin TFG.Typ c) =>
           (Exp g a -> Exp g b -> Exp g c -> Exp g t) ->
           Exp g a -> Exp g b -> Exp g c -> Chg (Exp g t)
cseOne3 k l m n  = case findTag l of
    Just(x , Exs1 ex tx) -> case getPrfHasSin tx of
               PrfHasSin -> if hasTag x ex m || hasTag x ex n then
                                chg (Let ex (absTag (Var Zro) x (sucAll ex) (sucAll (k l m n))))
                            else
                                k <$> cseOne l <*> cseOne m <*> cseOne n
    _ -> case findTag m of
     Just(x , Exs1 ex tx) -> case getPrfHasSin tx of
                PrfHasSin -> if hasTag x ex n then
                                 chg (Let ex (absTag (Var Zro) x (sucAll ex) (sucAll (k l m n))))
                             else
                                 k <$> cseOne l <*> cseOne m <*> cseOne n
     _ -> k <$> cseOne l <*> cseOne m <*> cseOne n

cseOne2 :: (HasSin TFG.Typ a, HasSin TFG.Typ b, HasSin TFG.Typ c) =>
           (Exp g a -> Exp g b -> Exp g c) ->
            Exp g a -> Exp g b -> Chg (Exp g c)
cseOne2 k m n = case findTag m of
      Just(x , Exs1 ex tx) -> case getPrfHasSin tx of
                 PrfHasSin -> if hasTag x ex n then
                                  chg (Let ex (absTag (Var Zro) x (sucAll ex) (sucAll (k m n))))
                              else
                                  k <$> cseOne m <*> cseOne n
      _ -> k <$> cseOne m <*> cseOne n

cseOne2F :: (HasSin TFG.Typ a, HasSin TFG.Typ b, HasSin TFG.Typ c) =>
           (Exp g a -> Exp (t ': g) b -> Exp g c) ->
            Exp g a -> Exp (t ': g) b -> Chg (Exp g c)
cseOne2F k m n = case findTag m of
      Just(x , Exs1 ex tx) -> case getPrfHasSin tx of
                 PrfHasSin -> if hasTag x (sucAll ex) n then
                                  chg (Let ex (absTag (Var Zro) x (sucAll ex) (sucAll (k m n))))
                              else
                                  k <$> cseOne m <*> cseOne n
      _ -> k <$> cseOne m <*> cseOne n

cseOne :: forall g a. HasSin TFG.Typ a => Exp g a -> Chg (Exp g a)
cseOne ee = let t = sin :: TFG.Typ a in case ee of
  ConI i                    -> pure (ConI i)
  ConB b                    -> pure (ConB b)
  ConF f                    -> pure (ConF f)
  App ef ea                 -> cseOne2 App ef ea
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
  Let el eb                 -> cseOne2F Let el eb
  Cmx er ei                 -> cseOne2 Cmx er ei
  Tag x e                   -> Tag x <$> cseOne e
  Mul er ei                 -> cseOne2 Mul er ei

remTag :: forall r t. Exp r t -> Exp r t
remTag ee = case ee of
  Tag _  e  -> remTag e
  _         -> $(genOverloaded 'ee ''Exp  ['Tag]
   (\ tt -> if
    | matchQ tt [t| Exp t t |]            -> [| remTag  |]
    | otherwise                           -> [| id |]))

remTheTag :: forall r t. String -> Exp r t -> Exp r t
remTheTag x ee = case ee of
  Tag y e
      | x == y    -> e
      | otherwise -> Tag y (remTheTag x e)
  _               -> $(genOverloaded 'ee ''Exp  ['Tag]
   (\ tt -> if
    | matchQ tt [t| Exp t t |]            -> [| remTheTag  x |]
    | otherwise                           -> [| id |]))


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
  Tag x e   -> Just (x , Exs1 e (sinTyp e))
  _         -> $(recAppMQ 'ee ''Exp (const [| Nothing |]) ['Tag]
    [| \ _x -> Nothing |] [| (<||>) |] [| (<||>) |] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| Exp (t ': t) t |]     -> [| findTag <=< prdAllM |]
    | matchQ tt [t| Exp t t |]            -> [| findTag |]
    | otherwise                           -> [| const Nothing |]))
{-
findTagF :: HasSin TFG.Typ b => Exp (a ': g) b -> Maybe (String , Exs1 (Exp g) TFG.Typ)
findTagF e = do e' <- prdAllM e
                findTag e'
-}
absTag :: forall t t' g.
          (HasSin TFG.Typ t , HasSin TFG.Typ t') =>
          Exp g t' -> String -> Exp g t' -> Exp g t -> Exp g t
absTag xx s ex ee = let t = sin :: TFG.Typ t in case ee of
  Tag x e
   | s == x    -> case eqlSin (sinTyp xx) t of
     Rgt Rfl   -> if eql e ex
                  then xx
                  else Tag x (absTag xx s ex e)
     _         -> impossible
   | otherwise -> Tag x (absTag xx s ex e)
  _            -> $(genOverloadedW 'ee ''Exp  ['Tag] (trvWrp 't)
   (\ tt -> if
     | matchQ tt [t| Exp (t ': t) t |]     -> [| absTag (sucAll xx) s (sucAll ex) |]
     | matchQ tt [t| Exp t t |]            -> [| absTag xx s ex |]
     | otherwise                           -> [| id |]))

hasTag :: (HasSin TFG.Typ t,HasSin TFG.Typ a) => String -> Exp g t -> Exp g a -> Bool
hasTag s ex ee = numTag s ex ee /= 0

numTag :: forall a t g. (HasSin TFG.Typ a , HasSin TFG.Typ t) =>
          String -> Exp g t -> Exp g a -> Int
numTag s ex ee = let t = sin :: TFG.Typ a in case ee of
  Tag x e
   | s == x    -> case eqlSin (sinTyp ex) t of
      Rgt Rfl  -> if eql ex e
                  then (1 :: Int)
                  else numTag s ex e
   | otherwise -> numTag s ex e
  _            -> $(recAppMQ 'ee ''Exp (const [| 0 :: Int |]) ['Tag]
     [| \ _x -> (0 :: Int) |] [| (+) |] [| (+) |] (trvWrp 't)
    (\ tt -> if
     | matchQ tt [t| Exp (t ': t) t |]     -> [| numTag s (sucAll ex) |]
     | matchQ tt [t| Exp a a |]            -> [| numTag s ex |]
     | otherwise                           -> [| const (0 :: Int)  |]))
