module QFeldspar.Expression.Feldspar.Conversions.Unquoting () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Expression.Feldspar.ADTUntypedNamed as FAUN
import qualified Language.Haskell.TH.Syntax          as TH

import QFeldspar.Conversion
import QFeldspar.Type.Feldspar.Conversion ()

(===) :: TH.Name -> TH.Name -> Bool
n1 === n2 = stripNameSpace n1 == stripNameSpace n2

mkDo :: [TH.Stmt] -> ErrM (FAUN.Exp TH.Name)
mkDo st = let ?r = () in case st of
  [TH.NoBindS e]                -> cnvImp e
  (TH.BindS (TH.VarP x) e : es) -> FAUN.App <$>
                                   (FAUN.App (FAUN.Var (TH.mkName "_bnd"))
                                            <$@> e)
                                   <*> ((\ y -> FAUN.Abs (x , y)) <$> mkDo es)
  (TH.NoBindS           e : es) -> FAUN.App <$>
                                   (FAUN.App (FAUN.Var (TH.mkName "_bnd"))
                                            <$@> e)
                                   <*> ((\ y -> FAUN.Abs
                                                (TH.mkName "__dummyb__" , y))
                                        <$> mkDo es)
  _                             -> fail ("Syntax Error!\n" ++ (show st))

mkLam :: TH.Exp -> ErrM (TH.Name , TH.Exp)
mkLam e = case e of
  TH.LamE [TH.VarP x] eb -> pure (x , eb)
  TH.LamE [p]         eb -> let v1 = genNewNam "__mkLam__"
                                {-# NOINLINE v1 #-}
                            in deepseq v1 $
                            let vv1 = stripNameSpace (TH.mkName v1) in
                             pure (vv1 , TH.CaseE (TH.VarE vv1)
                                           [TH.Match p (TH.NormalB eb) []])
  TH.LamE (x:xs)      eb -> mkLam (TH.LamE [x] (TH.LamE xs eb))
  _                      -> fail "Syntax Error!"


instance Cnv (TH.Exp , ()) (FAUN.Exp TH.Name) where
  cnv (ee , r) = let ?r = r in case ee of
    TH.ParensE e            -> cnvImp e
    TH.InfixE (Just el) ef
      (Just er)             -> cnvImp (TH.AppE (TH.AppE ef el) er)
    TH.LitE (TH.IntegerL i) -> pure (FAUN.ConI (fromInteger  i :: Int))
    TH.LitE (TH.RationalL i)-> pure (FAUN.ConF (fromRational i :: Flt))
    TH.ConE n
      | n == 'True          -> pure (FAUN.ConB True)
      | n == 'False         -> pure (FAUN.ConB False)
      | n == 'Nothing       -> pure FAUN.Non
      | otherwise           -> pure (FAUN.Var (stripNameSpace n))
    TH.VarE x               -> pure (FAUN.Var (stripNameSpace x))

    TH.LamE [TH.VarP x] eb  -> FAUN.Abs <$@> (x , eb)
    TH.LamE [p]         eb  -> let v1 = genNewNam "__UnqoutingL__"
                                   {-# NOINLINE v1 #-}
                               in deepseq v1 $
                               let vv1 = stripNameSpace (TH.mkName v1) in
                               cnvImp (TH.LamE [TH.VarP vv1]
                                       (TH.CaseE (TH.VarE vv1)
                                       [TH.Match p (TH.NormalB eb) []]))
    TH.LamE (x:xs)      eb  -> cnvImp (TH.LamE [x] (TH.LamE xs eb))

    TH.DoE stmts            -> mkDo stmts
    TH.AppE (TH.AppE (TH.ConE n) el) l
      | n === 'Vec           -> FAUN.AryV <$@> el  <*> (cnvImp =<< mkLam l)
    TH.AppE (TH.ConE n) e
      | n === 'Just          -> FAUN.Som  <$@> e
    TH.AppE (TH.VarE n) ea
      | n === 'fst           -> FAUN.Fst  <$@> ea
      | n === 'snd           -> FAUN.Snd  <$@> ea
      | n === 'arrLen        -> FAUN.Len  <$@> ea
    TH.AppE (TH.AppE
        (TH.VarE n) el) er
      | n === 'arrIx         -> FAUN.Ind  <$@> el <*@> er
      | n === 'cmx           -> FAUN.Cmx  <$@> el <*@> er
      | n === '(*)           -> FAUN.Mul  <$@> el <*@> er
    TH.AppE (TH.AppE
             (TH.VarE n) el)
            l
      | n === 'arr           -> FAUN.Ary  <$@> el  <*> (cnvImp =<< mkLam l)

    TH.AppE (TH.AppE
        (TH.AppE (TH.VarE n)
        l1)
        l2)
        ei
      | n === 'while        -> FAUN.Whl  <$> (cnvImp =<< mkLam l1)
                                         <*> (cnvImp =<< mkLam l2)
                                         <*@> ei
    TH.AppE ef ea           -> FAUN.App  <$@> ef <*@> ea
    TH.CondE ec et ef       -> FAUN.Cnd  <$@> ec <*@> et <*@> ef
    TH.TupE [ef , es]       -> FAUN.Tpl  <$@> ef <*@> es
    TH.LetE [TH.ValD (TH.VarP x) (TH.NormalB el) []] eb
                            -> FAUN.Let  <$@> el <*@> (x , eb)
    TH.CaseE ec [TH.Match (TH.TupP [TH.VarP xf , TH.VarP xs]) (TH.NormalB eb) []]
                            -> let v1 = genNewNam "__UnqoutingP1__"
                                   {-# NOINLINE v1 #-}
                               in deepseq v1 $ let vv1 = stripNameSpace (TH.mkName v1) in
                                  do ec' <- cnvImp ec
                                     eb' <- cnvImp eb
                                     pure (FAUN.Let ec' (vv1 ,
                                           FAUN.Let (FAUN.Fst (FAUN.Var vv1)) (xf ,
                                           FAUN.Let (FAUN.Snd (FAUN.Var vv1)) (xs ,
                                             eb'))))
    TH.CaseE ec [TH.Match (TH.ConP nl [TH.VarP xl , TH.VarP xf]) (TH.NormalB eb) []]
        | nl === 'Vec       -> let v1 = genNewNam "__UnqoutingC1__"
                                   {-# NOINLINE v1 #-}
                               in deepseq v1 $ let vv1 = stripNameSpace (TH.mkName v1) in
                               let v2 = genNewNam "__UnqoutingC2__"
                                   {-# NOINLINE v2 #-}
                               in deepseq v2 $ let vv2 = stripNameSpace (TH.mkName v2) in
                                  do ec' <- cnvImp ec
                                     eb' <- cnvImp eb
                                     pure (FAUN.Let ec' (vv1 ,
                                             FAUN.Let (FAUN.LenV (FAUN.Var vv1)) (xl ,
                                             FAUN.Let (FAUN.Abs (vv2 , FAUN.IndV (FAUN.Var vv1) (FAUN.Var vv2))) (xf ,
                                             eb'))))
    TH.CaseE ec [TH.Match (TH.ConP nl []) (TH.NormalB el) []
                ,TH.Match (TH.ConP nr [TH.VarP xr]) (TH.NormalB er) []]
        | nl === 'Nothing ,
          nr === 'Just      -> FAUN.May <$@> ec <*@> el
                               <*@> (xr , er)
    TH.SigE e t             -> FAUN.Typ <$@> t  <*@> e
    e                       -> fail  ("Syntax Error!\n" ++ (show e))

instance Cnv ((TH.Name , TH.Exp) , ()) (TH.Name , FAUN.Exp TH.Name) where
    cnv ((x , e) , r) = let ?r = r
                        in (,) <$> pure (stripNameSpace x) <*@> e

instance Cnv (FAUN.Exp TH.Name , ()) TH.Exp where
  cnv (ee , r) = let ?r = r in case ee of
    FAUN.ConI i             -> pure (TH.LitE (TH.IntegerL (toInteger i)))
    FAUN.ConB True          -> pure (TH.ConE 'True)
    FAUN.ConB False         -> pure (TH.ConE 'False)
    FAUN.ConF i             -> pure (TH.LitE (TH.RationalL (toRational i)))
    FAUN.Var n              -> pure (TH.VarE n)
    FAUN.Abs (x , eb)       -> TH.LamE [TH.VarP x] <$@> eb
    FAUN.App ef ea          -> TH.AppE <$@> ef <*@> ea
    FAUN.Cnd ec et ef       -> TH.CondE <$@> ec <*@> et <*@> ef
    FAUN.Whl (xc , ec)
             (xb , eb) ei   -> do ec' <- cnvImp ec
                                  eb' <- cnvImp eb
                                  ei' <- cnvImp ei
                                  pure (TH.AppE (TH.AppE
                                                 (TH.AppE (TH.VarE 'while)
                                                  (TH.LamE [TH.VarP xc] ec'))
                                                 (TH.LamE [TH.VarP xb] eb'))
                                        ei')
    FAUN.Tpl ef es          -> do ef' <- cnvImp ef
                                  es' <- cnvImp es
                                  pure (TH.TupE [ef' , es'])
    FAUN.Fst ea             -> TH.AppE (TH.VarE 'fst) <$@> ea
    FAUN.Snd ea             -> TH.AppE (TH.VarE 'snd) <$@> ea
    FAUN.Ary el (x , eb)    -> do el' <- cnvImp el
                                  eb' <- cnvImp eb
                                  pure (TH.AppE (TH.AppE (TH.VarE 'arr) el')
                                                 (TH.LamE [TH.VarP x] eb'))
    FAUN.Len ea             -> TH.AppE (TH.VarE 'arrLen) <$@> ea
    FAUN.Ind el ef          -> do el' <- cnvImp el
                                  ef' <- cnvImp ef
                                  pure (TH.AppE (TH.AppE (TH.VarE 'arrIx) el')
                                          ef')
    FAUN.AryV el (x , eb)   -> do el' <- cnvImp el
                                  eb' <- cnvImp eb
                                  pure (TH.AppE (TH.AppE (TH.ConE 'Vec) el')
                                                 (TH.LamE [TH.VarP x] eb'))
    FAUN.LenV ea            -> let v1 = genNewNam "__LenVQuoting1__"
                                   {-# NOINLINE v1 #-}
                               in deepseq v1 $
                               let v2 = genNewNam "__LenVQuoting2__"
                                   {-# NOINLINE v2 #-}
                               in deepseq v2 $
                                  do ea' <- cnvImp ea
                                     pure (TH.CaseE ea'
                                           [TH.Match (TH.ConP 'Vec
                                            [TH.VarP
                                             (stripNameSpace (TH.mkName v1)) ,
                                            TH.VarP
                                             (stripNameSpace (TH.mkName v2))])
                                              (TH.NormalB
                                               (TH.VarE (stripNameSpace
                                                       (TH.mkName v1)))) []])

    FAUN.IndV ea ei         -> let v1 = genNewNam "__IndVQuoting1__"
                                   {-# NOINLINE v1 #-}
                               in deepseq v1 $
                               let v2 = genNewNam "__IndVQuoting2__"
                                   {-# NOINLINE v2 #-}
                               in deepseq v2 $
                                  do ea' <- cnvImp ea
                                     ei' <- cnvImp ei
                                     pure (TH.CaseE ea'
                                           [TH.Match (TH.ConP 'Vec
                                            [TH.VarP
                                             (stripNameSpace (TH.mkName v1)) ,
                                            TH.VarP
                                             (stripNameSpace (TH.mkName v2))])
                                              (TH.NormalB
                                               (TH.AppE  (TH.VarE (stripNameSpace
                                                                (TH.mkName v2)))
                                                 ei')
                                              ) []])

    FAUN.Let el (x , eb)    -> do el' <- cnvImp el
                                  eb' <- cnvImp eb
                                  pure (TH.LetE [TH.ValD (TH.VarP x)
                                         (TH.NormalB el') []] eb')
    FAUN.Cmx er ei          -> do er' <- cnvImp er
                                  ei' <- cnvImp ei
                                  pure (TH.AppE (TH.AppE (TH.VarE 'cmx) er') ei')
    FAUN.Mul er ei          -> do er' <- cnvImp er
                                  ei' <- cnvImp ei
                                  pure (TH.AppE (TH.AppE (TH.VarE '(*)) er') ei')
    FAUN.Non                -> pure (TH.ConE 'Nothing)
    FAUN.Som e              -> TH.AppE (TH.ConE 'Just) <$@> e
    FAUN.May em en (x , es) -> do em' <- cnvImp em
                                  en' <- cnvImp en
                                  es' <- cnvImp es
                                  pure (TH.CaseE em'
                                       [TH.Match (TH.ConP 'Nothing [])
                                                 (TH.NormalB en') [],
                                        TH.Match (TH.ConP 'Just [TH.VarP x])
                                                 (TH.NormalB es') []])
    FAUN.Typ t e            -> TH.SigE <$@> e <*@> t
