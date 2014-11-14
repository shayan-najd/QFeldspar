module QFeldspar.TraversalGenerator where

import Prelude
import Control.Applicative (pure)
import Language.Haskell.TH.Syntax hiding (unQ)
import Language.Haskell.TH hiding (match)
import QFeldspar.ErrorMonad
import QFeldspar.Expression.TemplateHaskell ()

stripNameSpace :: Name -> Name
stripNameSpace (Name x _) = Name x NameS

(===) :: Name -> Name -> Bool
n1 === n2 = stripNameSpace n1 == stripNameSpace n2

unQ :: Q a -> a
unQ = frmRgt . runQ

getNameCount :: Con -> (Name, [Type])
getNameCount (NormalC n ts)   = (n , map snd ts)
getNameCount (ForallC _ _ c)  = getNameCount c
getNameCount _                = error "not supported"

nameConsT :: Name
nameConsT = (Name (OccName ":")
             (NameG DataName (PkgName "ghc-prim")
                        (ModName "GHC.Types")))

matchQ :: Type -> Q Type -> Bool
matchQ t t' = match t (unQ t')

match :: Type -> Type -> Bool
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

gen :: Name -> Name -> [Name] -> Q Exp -> Q Exp
gen ee n ns e = recAppQ ee n (return . ConE)
             ns (\ _ -> id) (const e)

genOverloaded :: Name -> Name -> [Name] -> (Type -> Q Exp) -> Q Exp
genOverloaded e n ns f = recAppQ e n (return . ConE)
                         ns (\ _ -> id) f

genOverloadedW :: Name -> Name -> [Name] -> (Name -> Q Exp -> Q Exp) ->
                  (Type -> Q Exp) -> Q Exp
genOverloadedW e n ns wf f = recAppQ e n (return . ConE)
                           ns wf f


recAppQ :: Name -> Name -> (Name -> Q Exp) -> [Name] ->
           (Name -> Q Exp -> Q Exp) -> (Type -> Q Exp) -> Q Exp
recAppQ e dn g ns wf fn = recApp e dn (unQ . g) ns
                        (\ n -> unQ . wf n . return)
                        (unQ . fn)

recApp :: Name -> Name -> (Name -> Exp) -> [Name] -> (Name -> Exp -> Exp) ->
          (Type -> Exp) -> Q Exp
recApp e dn g ns wf fn = recAppM e dn g ns
                         (unQ [| id |]) (unQ [| ($) |]) (unQ [| ($) |])
                                        wf fn
{-
biRecAppMQS :: Name -> Name -> String -> [Name] ->
               (Name -> Q Exp -> Q Exp) -> Q Exp
biRecAppMQS e dn g ns wf = recAppMQ e dn
   (\ n -> conE (mkName (g ++ "." ++ nameBase n)))
   ns [| pure |]
   (varE $ stripNameSpace $ mkName "<$@>")
   (varE $ stripNameSpace $ mkName "<*@>")
   wf
   (const [| id |])

-}
biRecAppMQ :: Name -> Name -> String -> Q Exp
biRecAppMQ e dn g =  biRecAppMQW e dn g [] (const id)

biRecAppMQW :: Name -> Name -> String -> [Name] ->
               (Name -> Q Exp -> Q Exp) -> Q Exp
biRecAppMQW e dn g ns wf = recAppMQ e dn
   (\ n -> conE (mkName (g ++ "." ++ nameBase n)))
   ns [| pure |]
   (varE $ stripNameSpace $ mkName "<$@>")
   (varE $ stripNameSpace $ mkName "<*@>")
   wf
   (const [| id |])

recAppMQ :: Name -> Name -> (Name -> Q Exp) -> [Name] -> Q Exp -> Q Exp ->
            Q Exp -> (Name -> Q Exp -> Q Exp) -> (Type -> Q Exp) ->
            Q Exp
recAppMQ e dn g ns o0 o1 o2 wf fn = recAppM e dn (unQ . g) ns
  (unQ o0) (unQ o1) (unQ o2) (\ n -> unQ . wf n . return) (unQ . fn)

recAppM :: Name -> Name -> (Name -> Exp) -> [Name] -> Exp -> Exp -> Exp ->
           (Name -> Exp -> Exp) ->
           (Type -> Exp) -> Q Exp
recAppM e dn g ns o0 o1 o2 wf fn = do
  TyConI (DataD _ _ _ ds _) <- reify dn
  return $ CaseE (VarE e)
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
             , not (n `elem` ns)]
