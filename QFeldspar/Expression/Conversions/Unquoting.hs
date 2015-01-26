module QFeldspar.Expression.Conversions.Unquoting () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Expression.ADTUntypedNamed as FAUN
import qualified Language.Haskell.TH.Syntax          as TH
import QFeldspar.Expression.Utils.TemplateHaskell
import QFeldspar.Conversion
import QFeldspar.Type.Conversion ()

mkDo :: [TH.Stmt] -> NamM ErrM (FAUN.Exp TH.Name)
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

instance Cnv (TH.Exp , ()) (FAUN.Exp TH.Name) where
  cnv (ee , r) = let ?r = r in case ee of
    TH.ParensE e            -> cnvImp e
    TH.InfixE (Just el) ef
      (Just er)             -> cnvImp (TH.AppE (TH.AppE ef el) er)
    TH.DoE stmts            -> mkDo stmts
    TH.LitE (TH.IntegerL i) -> pure (FAUN.Int  (fromInteger  i :: Int))
    TH.LitE (TH.RationalL i)-> pure (FAUN.ConF (fromRational i :: Flt))
    TH.ConE n
      | n === 'True         -> pure (FAUN.ConB True)
      | n === 'False        -> pure (FAUN.ConB False)
      | n === 'Nothing      -> pure FAUN.Non
      | n === 'Vec          -> let v1 = genNewNam "__UnqVec1__"
                                   {-# NOINLINE v1 #-}
                               in deepseq v1 $
                               let vv1 = stripNameSpace (TH.mkName v1) in
                               let v2 = genNewNam "__UnqVec2__"
                                   {-# NOINLINE v2 #-}
                               in deepseq v2 $
                               let vv2 = stripNameSpace (TH.mkName v2) in
                               pure (FAUN.Abs (vv1 ,
                                     FAUN.Abs (vv2 ,
                                     FAUN.AryV (FAUN.Var vv1)
                                               (FAUN.Var vv2))))
      | n === 'Just         -> let v1 = genNewNam "__UnqJust1__"
                                   {-# NOINLINE v1 #-}
                               in deepseq v1 $
                               let vv1 = stripNameSpace (TH.mkName v1) in
                               pure (FAUN.Abs (vv1 ,
                                     FAUN.Som (FAUN.Var vv1)))
      | otherwise           -> pure (FAUN.Var (stripNameSpace n))
    TH.VarE n
      | n === 'fst          -> let v1 = genNewNam "__Unqfst1__"
                                   {-# NOINLINE v1 #-}
                               in deepseq v1 $
                               let vv1 = stripNameSpace (TH.mkName v1) in
                               pure (FAUN.Abs (vv1 ,
                               FAUN.Fst (FAUN.Var vv1)))
      | n === 'snd          -> let v1 = genNewNam "__Unqsnd1__"
                                   {-# NOINLINE v1 #-}
                               in deepseq v1 $
                               let vv1 = stripNameSpace (TH.mkName v1) in
                               pure (FAUN.Abs (vv1 ,
                               FAUN.Snd (FAUN.Var vv1)))
      | n === 'arrLen       -> let v1 = genNewNam "__Unqlen1__"
                                   {-# NOINLINE v1 #-}
                               in deepseq v1 $
                               let vv1 = stripNameSpace (TH.mkName v1) in
                               pure (FAUN.Abs (vv1 ,
                               FAUN.Len (FAUN.Var vv1)))
      | n === 'arrIx        -> let v1 = genNewNam "__UnqInd1__"
                                   {-# NOINLINE v1 #-}
                               in deepseq v1 $
                               let vv1 = stripNameSpace (TH.mkName v1) in
                               let v2 = genNewNam "__UnqInd2__"
                                   {-# NOINLINE v2 #-}
                               in deepseq v2 $
                               let vv2 = stripNameSpace (TH.mkName v2) in
                               pure (FAUN.Abs (vv1 ,
                                     FAUN.Abs (vv2 ,
                                     FAUN.Ind (FAUN.Var vv1) (FAUN.Var vv2))))
      | n === 'cmx          -> let v1 = genNewNam "__UnqCmx1__"
                                   {-# NOINLINE v1 #-}
                               in deepseq v1 $
                               let vv1 = stripNameSpace (TH.mkName v1) in
                               let v2 = genNewNam "__UnqCmx2__"
                                   {-# NOINLINE v2 #-}
                               in deepseq v2 $
                               let vv2 = stripNameSpace (TH.mkName v2) in
                               pure (FAUN.Abs (vv1 ,
                                     FAUN.Abs (vv2 ,
                                     FAUN.Cmx (FAUN.Var vv1) (FAUN.Var vv2))))
      | n === '(*)          -> let v1 = genNewNam "__UnqMul1__"
                                   {-# NOINLINE v1 #-}
                               in deepseq v1 $
                               let vv1 = stripNameSpace (TH.mkName v1) in
                               let v2 = genNewNam "__UnqMul2__"
                                   {-# NOINLINE v2 #-}
                               in deepseq v2 $
                               let vv2 = stripNameSpace (TH.mkName v2) in
                               pure (FAUN.Abs (vv1 ,
                                     FAUN.Abs (vv2 ,
                                     FAUN.Mul (FAUN.Var vv1) (FAUN.Var vv2))))
      | n === 'arr          -> let v1 = genNewNam "__UnqAry1__"
                                   {-# NOINLINE v1 #-}
                               in deepseq v1 $
                               let vv1 = stripNameSpace (TH.mkName v1) in
                               let v2 = genNewNam "__Unqary2__"
                                   {-# NOINLINE v2 #-}
                               in deepseq v2 $
                               let vv2 = stripNameSpace (TH.mkName v2) in
                               pure (FAUN.Abs (vv1 ,
                                     FAUN.Abs (vv2 ,
                                     FAUN.Ary (FAUN.Var vv1) (FAUN.Var vv2))))
      | n === 'while        -> let v1 = genNewNam "__UnqWhl1__"
                                   {-# NOINLINE v1 #-}
                               in deepseq v1 $
                               let vv1 = stripNameSpace (TH.mkName v1) in
                               let v2 = genNewNam "__UnqWhl2__"
                                   {-# NOINLINE v2 #-}
                               in deepseq v2 $
                               let vv2 = stripNameSpace (TH.mkName v2) in
                               let v3 = genNewNam "__UnqWhl3__"
                                   {-# NOINLINE v3 #-}
                               in deepseq v3 $
                               let vv3 = stripNameSpace (TH.mkName v3) in
                               pure (FAUN.Abs (vv1 ,
                                     FAUN.Abs (vv2 ,
                                     FAUN.Abs (vv3 ,
                                     FAUN.Whl (FAUN.Var vv1) (FAUN.Var vv2)
                                              (FAUN.Var vv3)))))
      | otherwise           -> pure (FAUN.Var (stripNameSpace n))

    TH.LamE [TH.VarP x] eb  -> FAUN.Abs <$@> (x , eb)
    TH.LamE [p]         eb  -> let v1 = genNewNam "__UnqoutingL__"
                                   {-# NOINLINE v1 #-}
                               in deepseq v1 $
                               let vv1 = stripNameSpace (TH.mkName v1) in
                               cnvImp (TH.LamE [TH.VarP vv1]
                                       (TH.CaseE (TH.VarE vv1)
                                       [TH.Match p (TH.NormalB eb) []]))
    TH.LamE (x:xs)      eb  -> cnvImp (TH.LamE [x] (TH.LamE xs eb))

    TH.AppE (TH.AppE (TH.ConE n) el) l
      | n === 'Vec           -> FAUN.AryV <$@> el <*@> l
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
      | n === 'arr           -> FAUN.Ary  <$@> el <*@> er
    TH.AppE (TH.AppE
        (TH.AppE (TH.VarE n)
        l1) l2) ei
      | n === 'while        -> FAUN.Whl  <$@> l1 <*@> l2
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
          nr === 'Just      -> FAUN.May <$@> ec <*@> el <*@> (TH.LamE [TH.VarP xr] er)
    TH.SigE e t             -> FAUN.Typ <$@> t  <*@> e
    e                       -> fail  ("Syntax Error!\n" ++ (show e))

instance Cnv ((TH.Name , TH.Exp) , ()) (TH.Name , FAUN.Exp TH.Name) where
    cnv ((x , e) , r) = let ?r = r
                        in (,) <$> pure (stripNameSpace x) <*@> e
