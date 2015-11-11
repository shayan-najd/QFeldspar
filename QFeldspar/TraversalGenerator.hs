module QFeldspar.TraversalGenerator where

import Prelude
import Language.Haskell.TH.Syntax hiding (unQ)
import Language.Haskell.TH hiding (match)
import QFeldspar.Expression.Utils.TemplateHaskell
import Data.Char
import Control.Applicative ()

lowerFirst :: String -> String
lowerFirst []        = []
lowerFirst (x : xs)  = toLower x : xs

getNameCount :: Con -> (Name, [Type])
getNameCount (NormalC n ts)   = (n , map snd ts)
getNameCount (ForallC _ _ c)  = getNameCount c
getNameCount (InfixC tl n tr) = getNameCount (NormalC n [tl,tr])
getNameCount _                = error "not supported"

nameConsT :: Name
nameConsT = (Name (OccName ":")
             (NameG DataName (PkgName "ghc-prim")
                        (ModName "GHC.Types")))

matchQ :: Type -> Q Type -> Bool
matchQ t t' = match t (unQ t')

match :: Type -> Type -> Bool
match (SigT a _)            a'                    = match a a'
match _                     (VarT _)              = True
match (ConT n)              (PromotedT n')        = n == n'
match (PromotedT n)         (ConT n')             = n == n'
match (ConT n)              (ConT n')             = n == n'
match (AppT a b)            (AppT a' b')          = match a a' &&
                                                    match b b'
match PromotedConsT         PromotedConsT         = True
match (PromotedT n) PromotedConsT
    | n == nameConsT                              = True
match PromotedConsT         (PromotedT n)
    | n == nameConsT                              = True
match PromotedNilT          PromotedNilT          = True
match ArrowT                ArrowT                = True
match ListT                 ListT                 = True
match (TupleT n)            (TupleT n')           = n == n'
match (PromotedT n)         (PromotedT n')        = n == n'
match _                     _                     = False

{-
gen :: Name -> Name -> [Name] -> Q Exp -> Q Exp
gen ee n ns e = recAppQ ee n (return . ConE)
             ns (\ _ -> id) (const e)
-}

genOverloaded :: Name -> Name -> [Name] ->
                 (Type -> Q Exp) -> Q Exp
genOverloaded e t ns f = recAppMQ e t
                         (return . ConE)
                         ns
                         [| id |]
                         [| ($) |]
                         [| ($) |]
                         f

genOverloadedM :: Name -> Name -> [Name] ->
                  (Type -> Q Exp) -> Q Exp
genOverloadedM e t ns f = recAppMQ e t
                          (return . ConE)
                          ns
                          [| pure |]
                          [| (<$>) |]
                          [| (<*>) |]
                          f

biGenOverloaded :: Name -> Name -> String -> [Name] ->
                   (Type -> Q Exp) -> Q Exp
biGenOverloaded e t g ns f = recAppMQ e t
                           (\ n -> conE (mkName (g ++ "." ++ ( (nameBase n)))))
                           ns
                           [| id |]
                           [| ($) |]
                           [| ($) |]
                           f

biGenOverloadedM :: Name -> Name -> String -> [Name] ->
                  (Type -> Q Exp) -> Q Exp
biGenOverloadedM e t g ns f = recAppMQ e t
                               (\ n -> conE (mkName (g ++ "." ++ ( (nameBase n)))))
                               ns
                               [| pure |]
                               [| (<$>) |]
                               [| (<*>) |]
                               f

biGenOverloadedL :: Name -> Name -> String -> [Name] ->
                   (Type -> Q Exp) -> Q Exp
biGenOverloadedL e t g ns f = recAppMQ e t
                           (\ n -> varE (mkName (g ++ "." ++ (lowerFirst(nameBase n)))))
                           ns
                           [| id |]
                           [| ($) |]
                           [| ($) |]
                           f


biGenOverloadedML :: Name -> Name -> String -> [Name] ->
                  (Type -> Q Exp) -> Q Exp
biGenOverloadedML e t g ns f = recAppMQ e t
                               (\ n -> varE (mkName (g ++ "." ++ (lowerFirst(nameBase n)))))
                               ns
                               [| pure |]
                               [| (<$>) |]
                               [| (<*>) |]
                               f

biGenOverloadedMWL :: Name -> Name -> String -> [Name] ->
                  (Type -> Q Exp) -> Q Exp
biGenOverloadedMWL e t g ns f = recAppMQ e t
                                (\ n -> varE (mkName (g ++ "." ++ (lowerFirst(nameBase n)))))
                                ns
                                [| pure |]
                                [| (<$>) |]
                                [| (<*>) |]
                                f

recAppMQ :: Name -> Name -> (Name -> Q Exp) -> [Name] -> Q Exp -> Q Exp ->
            Q Exp -> (Type -> Q Exp) -> Q Exp
recAppMQ e dn g ns o0 o1 o2 fn = recAppM e dn (unQ . g) ns
                                 (unQ o0) (unQ o1) (unQ o2)
                                 (\ _ -> unQ . return) (unQ . fn)

recAppM :: Name -> Name -> (Name -> Exp) -> [Name] -> Exp -> Exp -> Exp ->
           (Name -> Exp -> Exp) ->
           (Type -> Exp) -> Q Exp
recAppM e dn g ns o0 o1 o2 wf fn = do
  TyConI (DataD _ _ _ ds _) <- reify dn
  return $ CaseE (VarE e) $
             [Match (ConP n
                     [VarP (mkName ("_x" ++ show i))
                     | i <- [0.. length tl-1]])
              (NormalB
               (case length tl of
                  0 -> wf n (AppE o0 (g n))
                  1 -> wf n (InfixE
                       (Just (g n))
                       o1
                       (Just $ AppE
                            (fn (tl!!0)) (VarE (mkName "_x0"))))
                  m -> wf n (foldl (\ es i -> InfixE
                               (Just es)
                               o2
                               (Just $ AppE
                                     (fn (tl!!i))
                                     (VarE (mkName ("_x" ++ show i)))))
                        (InfixE
                         (Just (g n))
                         o1
                         (Just$ AppE
                                 (fn (tl!!0)) (VarE (mkName "_x0"))))
                        [1 .. m-1])
               )
              ) []
             | d <- ds, let (n , tl) = getNameCount d
             , not (n `elem` ns)] ++
             [ Match (ConP n
                     [VarP (mkName ("_x" ++ show i))
                     | i <- [0.. length tl-1]])
                (NormalB (AppE (VarE 'error) (LitE (StringL "Impossible!")))) []
             | d <- ds, let (n , tl) = getNameCount d
             , n `elem` ns]
