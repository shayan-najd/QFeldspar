{-# OPTIONS_GHC -w #-}
module QFeldspar.Expression.Conversions.EtaPrims (etaPrms) where

import QFeldspar.MyPrelude
import Prelude (Int)
import QFeldspar.Expression.ADTUntypedNamed
import qualified QFeldspar.Environment.Map    as EM
import qualified QFeldspar.Environment.Scoped as ES
import qualified QFeldspar.Environment.Typed  as ET
import QFeldspar.Variable.Conversion    ()
import qualified Language.Haskell.TH.Syntax as TH
import QFeldspar.Expression.Utils.TemplateHaskell
import qualified QFeldspar.Type.GADT as TG
import QFeldspar.Singleton
import QFeldspar.ChangeMonad

arity :: TG.Typ a -> Int
arity (TG.Arr _ b) = 1 + arity b
arity _            = 0

arities :: ET.Env TG.Typ s -> ES.Env (Len s) TH.Name ->
         EM.Env TH.Name Int
arities ET.Emp         ES.Emp        = EM.Emp
arities (ET.Ext t ts)  (ES.Ext n ns) = EM.Ext (n , arity t) (arities ts ns)
arities _              _             = impossible

etaPrms :: (Functor m , Monad m) =>
           ET.Env TG.Typ s -> ES.Env (Len s) TH.Name -> Exp TH.Name -> NamM m (Exp TH.Name)
etaPrms s g ee = etaPrms' (arities s g) ee

etaPrms' :: (Functor m , Monad m) => EM.Env TH.Name Int -> Exp TH.Name -> NamM m (Exp TH.Name)
etaPrms' g ee = let prms = tilNotChg (mkPrms (fmap fst g)) ee
                in expandPrms g prms

isIn :: TH.Name -> [TH.Name] -> Bool
isIn _ []     = False
isIn x (y : ys)
  | x === y   = True
  | otherwise = isIn x ys

delEP :: TH.Name -> [TH.Name] -> [TH.Name]
delEP _ []      = []
delEP x (y : ys)
  | x === y     = delEP x ys
  | otherwise   = y : delEP x ys

delEM :: TH.Name -> EM.Env TH.Name a -> EM.Env TH.Name a
delEM _ []      = []
delEM x ((y,y') : ys)
  | x === y     = delEM x ys
  | otherwise   = (y , y') : delEM x ys


mkPrms :: forall a.[TH.Name] -> Exp TH.Name -> Chg (Exp TH.Name)
mkPrms g ee = case ee of
  Var  x
    | x `isIn` g    -> chg (Prm x [])
  App  (Prm x ns) m -> chg (Prm x (ns ++ [m]))
  _                 -> $(genOverloadedM 'ee ''Exp []
   (\ tt -> if
     | matchQ tt [t| Exp a   |]     -> [| mkPrms g |]
     | matchQ tt [t| (a , Exp a) |] -> [| \ (x , e) -> (,) <$> pure x <*> mkPrms (delEP x g) e |]
     | matchQ tt [t| [Exp a] |]     -> [| mapM (mkPrms g) |]
     | otherwise                    -> [| pure   |]))

expandPrms :: forall a m. (Functor m  , Monad m) => EM.Env TH.Name Int -> Exp TH.Name -> NamM m (Exp TH.Name)
expandPrms g ee = case ee of
  Prm x ns  -> case EM.get x g of
    Just s | length ns < s  -> do xs <- mapM (const newTHVar) [1..s - length ns]
                                  expandPrms g (foldr (curry Abs) (Prm x (ns ++ fmap Var xs)) xs)
    Just s | length ns == s -> Prm x <$> mapM (expandPrms g) ns
    Just s | length ns >  s -> fail ("Scope Error: the primitive '" ++ show x ++ "' has too many arguments.")
    _                       -> fail "Scope Error: undefined primitives!"
  _         -> $(genOverloadedM 'ee ''Exp []
   (\ tt -> if
     | matchQ tt [t| Exp a |]       -> [| expandPrms g |]
     | matchQ tt [t| (a , Exp a) |] -> [| \ (x , e) -> do e' <- expandPrms (delEM x g) e
                                                          pure (x , e') |]
     | otherwise                    -> [| pure |]))

newTHVar :: Monad m => NamM m TH.Name
newTHVar = do v1 <- newVar
              return (stripNameSpace (TH.mkName v1))
