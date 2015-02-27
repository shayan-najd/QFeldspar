module QFeldspar.Expression.Conversions.Unquoting () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Expression.ADTUntypedNamed as FAUN
import qualified Language.Haskell.TH.Syntax          as TH
import QFeldspar.Expression.Utils.TemplateHaskell
import QFeldspar.Conversion
import QFeldspar.Type.Conversion ()

newTHVar :: NamM ErrM TH.Name
newTHVar = do v1 <- newVar
              return (stripNameSpace (TH.mkName v1))

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
      | n === 'Vec          -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (FAUN.Abs (v1 ,
                                     FAUN.Abs (v2 ,
                                     FAUN.AryV (FAUN.Var v1)
                                               (FAUN.Var v2))))
      | n === 'Just         -> do v1 <- newTHVar
                                  pure (FAUN.Abs (v1 ,
                                     FAUN.Som (FAUN.Var v1)))
      | otherwise           -> pure (FAUN.Var (stripNameSpace n))
    TH.VarE n
      | n === 'save         -> do v1 <- newTHVar
                                  pure (FAUN.Abs (v1 ,
                                                  FAUN.Mem (FAUN.Var v1)))
      | n === 'fst          -> do v1 <- newTHVar
                                  pure (FAUN.Abs (v1 ,
                                                  FAUN.Fst (FAUN.Var v1)))
      | n === 'snd          -> do v1 <- newTHVar
                                  pure (FAUN.Abs (v1 ,
                                                  FAUN.Snd (FAUN.Var v1)))
      | n === 'lnArr        -> do v1 <- newTHVar
                                  pure (FAUN.Abs (v1 ,
                                                  FAUN.Len (FAUN.Var v1)))
      | n === 'ixArr        -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (FAUN.Abs (v1 ,
                                     FAUN.Abs (v2 ,
                                     FAUN.Ind (FAUN.Var v1) (FAUN.Var v2))))
      | n === 'cmx          -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (FAUN.Abs (v1 ,
                                     FAUN.Abs (v2 ,
                                     FAUN.Cmx (FAUN.Var v1) (FAUN.Var v2))))
      | n === '(*)          -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (FAUN.Abs (v1 ,
                                     FAUN.Abs (v2 ,
                                     FAUN.Mul (FAUN.Var v1) (FAUN.Var v2))))
      | n === '(+)          -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (FAUN.Abs (v1 ,
                                     FAUN.Abs (v2 ,
                                     FAUN.Add (FAUN.Var v1) (FAUN.Var v2))))
      | n === '(-)          -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (FAUN.Abs (v1 ,
                                     FAUN.Abs (v2 ,
                                     FAUN.Sub (FAUN.Var v1) (FAUN.Var v2))))
      | n === '(==)         -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (FAUN.Abs (v1 ,
                                     FAUN.Abs (v2 ,
                                     FAUN.Eql (FAUN.Var v1) (FAUN.Var v2))))
      | n === '(<)          -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (FAUN.Abs (v1 ,
                                     FAUN.Abs (v2 ,
                                     FAUN.Ltd (FAUN.Var v1) (FAUN.Var v2))))
      | n === 'mkArr        -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (FAUN.Abs (v1 ,
                                     FAUN.Abs (v2 ,
                                     FAUN.Ary (FAUN.Var v1) (FAUN.Var v2))))
      | n === 'while        -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  v3 <- newTHVar
                                  pure (FAUN.Abs (v1 ,
                                     FAUN.Abs (v2 ,
                                     FAUN.Abs (v3 ,
                                     FAUN.Whl (FAUN.Var v1) (FAUN.Var v2)
                                              (FAUN.Var v3)))))
      | otherwise           -> pure (FAUN.Var (stripNameSpace n))

    TH.LamE [TH.VarP x] eb  -> FAUN.Abs <$@> (x , eb)
    TH.LamE [p]         eb  -> do v1 <- newTHVar
                                  cnvImp (TH.LamE [TH.VarP v1]
                                       (TH.CaseE (TH.VarE v1)
                                       [TH.Match p (TH.NormalB eb) []]))
    TH.LamE (x:xs)      eb  -> cnvImp (TH.LamE [x] (TH.LamE xs eb))

    TH.AppE (TH.AppE (TH.ConE n) el) l
      | n === 'Vec           -> FAUN.AryV <$@> el <*@> l
    TH.AppE (TH.ConE n) e
      | n === 'Just          -> FAUN.Som  <$@> e
    TH.AppE (TH.VarE n) ea
      | n === 'save          -> FAUN.Mem  <$@> ea
      | n === 'fst           -> FAUN.Fst  <$@> ea
      | n === 'snd           -> FAUN.Snd  <$@> ea
      | n === 'lnArr         -> FAUN.Len  <$@> ea
    TH.AppE (TH.AppE
        (TH.VarE n) el) er
      | n === 'ixArr         -> FAUN.Ind  <$@> el <*@> er
      | n === 'cmx           -> FAUN.Cmx  <$@> el <*@> er
      | n === '(*)           -> FAUN.Mul  <$@> el <*@> er
      | n === '(+)           -> FAUN.Add  <$@> el <*@> er
      | n === '(-)           -> FAUN.Sub  <$@> el <*@> er
      | n === '(==)          -> FAUN.Eql  <$@> el <*@> er
      | n === '(<)           -> FAUN.Ltd  <$@> el <*@> er
      | n === 'mkArr         -> FAUN.Ary  <$@> el <*@> er
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
                            -> do v1 <- newTHVar
                                  ec' <- cnvImp ec
                                  eb' <- cnvImp eb
                                  pure (FAUN.Let ec' (v1 ,
                                           FAUN.Let (FAUN.Fst (FAUN.Var v1)) (xf ,
                                           FAUN.Let (FAUN.Snd (FAUN.Var v1)) (xs ,
                                             eb'))))
    TH.CaseE ec [TH.Match (TH.ConP nl [TH.VarP xl , TH.VarP xf]) (TH.NormalB eb) []]
        | nl === 'Vec       -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  ec' <- cnvImp ec
                                  eb' <- cnvImp eb
                                  pure (FAUN.Let ec' (v1 ,
                                             FAUN.Let (FAUN.LenV (FAUN.Var v1)) (xl ,
                                             FAUN.Let (FAUN.Abs (v2 , FAUN.IndV (FAUN.Var v1) (FAUN.Var v2))) (xf ,
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
