{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Utils.MiniFeldspar where

import QFeldspar.MyPrelude hiding (foldl)
import QFeldspar.Expression.Utils.Common
import QFeldspar.Expression.MiniFeldspar
import QFeldspar.Variable.Typed
import qualified QFeldspar.Environment.Typed as ET
import qualified QFeldspar.Type.GADT as TG
import QFeldspar.Singleton

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
  Prm v es -> Prm (f v) (ET.fmap (mapVar f g) es)
  _         ->  $(genOverloaded 'ee ''Exp ['Prm]
   (\ t -> if
    | matchQ t [t| Exp t t -> Exp t t |] ->
        [| \ ff -> mapVar f g . ff . mapVar g f |]
    | matchQ t [t| Exp t t |]          ->
        [| mapVar f g |]
    | otherwise                        ->
        [| id |]))

absTmp :: forall r t t'. (TG.Type t', TG.Type t) =>
          Exp r t' -> String -> Exp r t -> Exp r t
absTmp xx s ee = let t = sin :: TG.Typ t in case ee of
  Prm v es     -> Prm v (TG.mapC (absTmp xx s) es)
  Tmp x
    | s == x    -> case eqlSin (sinTyp xx) (sin :: TG.Typ t) of
      Rgt Rfl   -> xx
      _         -> ee
    | otherwise -> ee
  _             -> $(genOverloadedW 'ee ''Exp  ['Prm,'Tmp] (trvWrp 't)
   (\ tt -> if
    | matchQ tt [t| Exp t t -> Exp t t |] -> [| (absTmp xx s .) |]
    | matchQ tt [t| Exp t t |]            -> [| absTmp xx s |]
    | otherwise                           -> [| id |]))

remTag :: forall r t. Exp r t -> Exp r t
remTag ee = case ee of
  Prm v es -> Prm v (TG.mapC remTag es)
  Tag _  e  -> remTag e
  _         -> $(genOverloaded 'ee ''Exp  ['Prm,'Tag]
   (\ tt -> if
    | matchQ tt [t| Exp t t -> Exp t t |] -> [| remTagF |]
    | matchQ tt [t| Exp t t |]            -> [| remTag  |]
    | otherwise                           -> [| id |]))

remTagF :: (Exp r a -> Exp r b) -> (Exp r a -> Exp r b)
remTagF = (remTag .)

-- shared :: TH.Q TH.Exp
-- shared = [| Tag $(do s <- fmap show (TH.newName "v")
   --                  [|s|]) |]
