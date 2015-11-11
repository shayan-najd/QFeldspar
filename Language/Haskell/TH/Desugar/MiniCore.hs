{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Haskell.TH.Desugar.MiniCore where

import qualified Language.Haskell.TH as TH
import Prelude
import Control.Monad
import Control.Applicative ()
import Data.Graph
import Data.Monoid ()
import Data.Data
import Data.Generics (everywhereM, mkM)
import Data.Function (fix)
import qualified Data.Set as Set

import Language.Haskell.TH (Name,Lit)
import Language.Haskell.TH.Syntax (Quasi(qNewName),mkName)

import Language.Haskell.TH.Desugar

{-
-- todo:
- Desugar Patterns
- Reorder Matches and add missing patterns

-- not todo:
- no static
- no kind
- case v of {~p -> e; _ -> e' }
   = (\x1 . . . xn -> e ) (case v of { p-> x1 }) . . . (case v of { p -> xn})
-}

data MExp = MVarE  Name
          | MConE  Name
          | MLitE  Lit
          | MAppE  MExp MExp
          | MLamE  Name MExp
          | MCaseE MExp [(DPat , MExp)]
          | MLetE  Name MExp MExp
          | MSigE  MExp DType
          deriving (Show, Typeable, Data)

dsDExp :: DsMonad q => DExp -> q MExp
dsDExp ee = case ee of
  DVarE x        -> pure (MVarE x)
  DConE x        -> pure (MConE x)
  DLitE a        -> pure (MLitE a)
  DAppE l m      -> MAppE <$> dsDExp l <*> dsDExp m
  DSigE n t      -> MSigE <$> dsDExp n <*> pure t
  DLamE []     _ -> fail "Impossible!"
  DLamE [x]    n -> MLamE x <$> dsDExp n
  DLamE (x:xs) n -> MLamE x <$> dsDExp (DLamE xs n)
  DCaseE l ms    -> MCaseE  <$> dsDExp l
                            <*> sequence
                                [ do e' <- dsDExp e
                                     return (p , e')
                                | DMatch p e <- ms]
  DStaticE _     -> fail "Static Expressions are not supported!"
  DLetE [DValD (DVarPa x) m] n
      | not (x `Set.member` freeVars m) -> MLetE x <$> dsDExp m <*> dsDExp n
  DLetE _  _     -> dsDExp =<< depAnalysis =<< dsDLetDecAll ee

isInfixD :: DLetDec -> Bool
isInfixD (DInfixD _ _) = True
isInfixD _             = False

isSigD :: DLetDec -> Bool
isSigD (DSigD _ _) = True
isSigD _           = False

isPVar :: DPat -> Bool
isPVar (DVarPa _) = True
isPVar _          = False

dsDLetDecAll :: DsMonad q => DExp -> q DExp
dsDLetDecAll = everywhereM (mkM dsDLetDecOne)

dsDLetDecOne :: DsMonad q => DExp -> q DExp
dsDLetDecOne (DLetE ds e) = DLetE <$> dsDLetDecs ds <*> pure e
dsDLetDecOne e            = pure e

dsDLetDecs :: DsMonad q => [DLetDec] -> q [DLetDec]
dsDLetDecs = dsPatBinds . dsSig <=< mapM dsFun . filter (not . isInfixD)

dsFun :: DsMonad q => DLetDec -> q DLetDec
dsFun (DFunD _ [])                       = error "Impossible!"
dsFun (DFunD n dcss@(DClause pps _ : _)) =
  do xs <- sequence [ qNewName ("_x" ++ show i)
           | i <- [0 .. (length pps - 1)]]
     return $ DValD (DVarPa n) (
      DLamE xs
       (if length pps == 1
        then (DCaseE (DVarE (head xs))
                  [ DMatch p e
                  | DClause [p] e <- dcss])
        else (DCaseE (tupExp (map DVarE xs))
                  [ DMatch (tupPat ps) e
                  | DClause ps e <- dcss])))
dsFun d                                 = pure d

dsSig ::  [DLetDec] ->  [DLetDec]
dsSig ds = foldl (\ x f -> f x)  (filter (not . isSigD) ds)
           [addType n t | DSigD n t <- ds]

dsPatBinds :: DsMonad q => [DLetDec] -> q [DLetDec]
dsPatBinds []       = return []
dsPatBinds ((DValD p e) : ds)
  | not (isPVar p)  = do xB <- qNewName "_xB"
                         fmap ((DValD (DVarPa xB) e :
                               [ DValD (DVarPa xi)
                                       (DCaseE (DVarE xB)
                                       [DMatch (DTildePa p) (DVarE xi)])
                               | xi <- Set.toList (boundNamesDPat p)]) ++) (dsPatBinds ds)
dsPatBinds (d : ds) = fmap (d :) (dsPatBinds ds)

addType :: Name -> DType -> [DLetDec] -> [DLetDec]
addType _ _ [] = []
addType n t (d@(DValD (DVarPa m) e) : ds)
  | n == m     = DValD (DVarPa m) (DSigE e t) : ds
  -- assuming there is only one declaration with the same name
  | otherwise  = d : addType n t ds
addType _ _ _  = error "Impossible!"

depAnalysis :: DsMonad q => DExp -> q DExp
depAnalysis (DLetE ds e) = (flatten e . toSCP . toGraph) ds
depAnalysis _              = error "Bad input!"

toGraph :: [DLetDec] -> [((Name, DExp), Name, [Name])]
toGraph ds =  [((n,e) , n , Set.toList (freeVars e))
              | DValD (DVarPa n) e <- ds]

toSCP :: [((Name , DExp), Name, [Name])] -> [SCC (Name , DExp)]
toSCP = stronglyConnComp

fixN :: Name
fixN = 'fix

tupPat :: [DPat] -> DPat
tupPat []  = DConPa '() []
tupPat [_] = error "Tuples cannot be of arity one!"
tupPat ps  = DConPa (mkName ("("++ [','|_ <- tail ps] ++")")) ps
--tupPat []     = DConPa '() []
--tupPat (n:ns) = DConPa '(,) [DVarPa n,tupPat ns]

tupExp :: [DExp] -> DExp
tupExp []  = DConE '()
tupExp [_] = error "Tuples cannot be of arity one!"
tupExp es  = foldl DAppE (DConE (mkName ("("++ [','|_ <- tail es] ++")"))) es
   --  foldr (\ x y -> DAppE (DAppE (DConE '(,)) x) y)
   --         (DConE '())

flatten :: DsMonad q => DExp -> [SCC (Name , DExp)] -> q DExp
flatten eb []                           = pure eb
flatten eb (CyclicSCC [(n , e)] : sccs) =
  DLetE [DValD (DVarPa n)
               (DAppE (DVarE fixN)
               (DLamE [n] e))] <$>
            flatten eb sccs
flatten eb (CyclicSCC es : sccs) =
  do x0 <- qNewName "_x0"
     x1 <- qNewName "_x1"
     mtch <- DMatch (tupPat (map (DVarPa . fst) es)) <$>
             (flatten eb sccs)
     return $ DLetE [DValD (DVarPa x0)
             (DAppE (DVarE fixN)
                    (DLamE [x1]
                      (DCaseE (DVarE x1)
                      [DMatch (tupPat (map (DVarPa . fst) es))
                              (tupExp (map snd es))])))]
               (DCaseE (DVarE x0)
                        [mtch])
flatten eb ((AcyclicSCC (n , e)) : sccs) = DLetE [DValD (DVarPa n) e] <$> flatten eb sccs

rename :: Name -> Name -> DExp -> DExp
rename x x' ee = case ee of
  DVarE y
    | y == x      -> DVarE x'
    | otherwise   -> ee
  DConE _       -> ee
  DLitE _       -> ee
  DAppE l m     -> DAppE (rename x x' l) (rename x x' m)
  DLamE xs n
    | x `elem` xs -> ee
    | otherwise   -> DLamE xs (rename x x' n)
  DCaseE l ms   -> DCaseE (rename x x' l)
                   [if x `Set.member` (boundNamesDPat p)
                    then DMatch p e
                    else DMatch p (rename x x' e)
                   | DMatch p e <- ms]
  DLetE ds n
    | x `Set.member` (boundNamesDLetDec ds) -> ee
    | otherwise -> DLetE [DValD p (rename x x' m) | DValD p m <- ds]
                   (rename x x' n)
  DSigE m t     -> DSigE (rename x x' m) t
  DStaticE m    -> rename x x' m

freeVars :: DExp -> Set.Set Name
freeVars ee = case ee of
  DVarE x     -> Set.singleton x
  DConE _     -> Set.empty
  DLitE _     -> Set.empty
  DAppE l m   -> freeVars l `Set.union` freeVars m
  DLamE xs n  -> freeVars n `Set.difference` Set.fromList xs
  DCaseE l ms -> freeVars l `Set.union`
                   (mconcat [freeVars e `Set.difference` boundNamesDPat p | DMatch p e <- ms])
  DLetE ds n  -> (freeVars n `Set.union`
                    (mconcat [freeVars m | DValD _ m <- ds]))
                    `Set.difference` boundNamesDLetDec ds
  DSigE m _   -> freeVars m
  DStaticE m  -> freeVars m

boundNamesDLetDec :: [DLetDec] -> Set.Set Name
boundNamesDLetDec ds = Set.fromList [n | DValD (DVarPa n) _ <- ds]

boundNamesDPat :: DPat -> Set.Set Name
boundNamesDPat pp = case pp of
  DLitPa _    -> Set.empty
  DVarPa x    -> Set.singleton x
  DConPa _ ps -> mconcat (map boundNamesDPat ps)
  DTildePa p  -> boundNamesDPat p
  DBangPa  p  -> boundNamesDPat p
  DWildPa     -> Set.empty

mexpToDExp :: MExp -> DExp
mexpToDExp ee = case ee of
  MVarE x     -> DVarE x
  MConE x     -> DConE x
  MLitE a     -> DLitE a
  MAppE l m   -> DAppE (mexpToDExp l) (mexpToDExp m)
  MSigE n t   -> DSigE (mexpToDExp n) t
  MLamE x n   -> DLamE [x] (mexpToDExp n)
  MCaseE l ms -> DCaseE (mexpToDExp l)
                        [DMatch p (mexpToDExp e) | (p , e) <- ms]
  MLetE x m n -> DLetE [DValD (DVarPa x) (mexpToDExp m)] (mexpToDExp n)

instance Desugar TH.Exp MExp where
  desugar = dsDExp <=< dsExp
  sweeten = expToTH . mexpToDExp
