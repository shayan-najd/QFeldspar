{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Utils.MiniFeldspar where

import QFeldspar.MyPrelude hiding (foldl)
import QFeldspar.Expression.Utils.Common
import QFeldspar.Expression.MiniFeldspar
import QFeldspar.Variable.Typed
import qualified QFeldspar.Environment.Typed as ET
import qualified QFeldspar.Type.GADT as TFG
import QFeldspar.Singleton
import qualified Language.Haskell.TH as TH

tagFree :: Exp r t -> Exp r t
tagFree (Tag _ e) = tagFree e
tagFree e         = e

pattern TF e <- (tagFree -> e)

sucAll :: Exp r t' -> Exp (t ': r) t'
sucAll = mapVar Suc prd

prdAll :: Exp (t ': r) t' -> Exp r t'
prdAll = mapVar prd Suc

mapVar :: forall r r' t.
          (forall t'. Var r  t' -> Var r' t') ->
          (forall t'. Var r' t' -> Var r  t') ->
          Exp r t -> Exp r' t
mapVar f g ee = case ee of
  AppV v es -> AppV (f v) (ET.fmap (mapVar f g) es)
  _         ->  $(genOverloaded 'ee ''Exp ['AppV]
   (\ t -> if
    | matchQ t [t| Exp t t -> Exp t t |] ->
        [| \ ff -> mapVar f g . ff . mapVar g f |]
    | matchQ t [t| Exp t t |]          ->
        [| mapVar f g |]
    | otherwise                        ->
        [| id |]))

absTmp :: forall r t t'. (HasSin TFG.Typ t', HasSin TFG.Typ t) =>
          Exp r t' -> String -> Exp r t -> Exp r t
absTmp xx s ee = let t = sin :: TFG.Typ t in case ee of
  AppV v es     -> AppV v (TFG.mapC (sinTyp v) (absTmp xx s) es)
  Tmp x
    | s == x    -> case eqlSin (sinTyp xx) (sin :: TFG.Typ t) of
      Rgt Rfl   -> xx
      _         -> ee
    | otherwise -> ee
  _             -> $(genOverloadedW 'ee ''Exp  ['AppV,'Tmp] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| Exp t t -> Exp t t |] -> [| (absTmp xx s .) |]
    | matchQ tt [t| Exp t t |]            -> [| absTmp xx s |]
    | otherwise                           -> [| id |]))

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

shared :: TH.Q TH.Exp
shared = [| Tag $(do s <- fmap show (TH.newName "v")
                     [|s|]) |]
