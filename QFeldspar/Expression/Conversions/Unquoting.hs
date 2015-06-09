module QFeldspar.Expression.Conversions.Unquoting () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Expression.ADTUntypedNamed as AUN
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Desugar as  DTH
import qualified GHC.Types
import Language.Haskell.TH.Instances ()
import QFeldspar.Expression.Utils.TemplateHaskell
import QFeldspar.Conversion
import QFeldspar.Type.Conversion ()

instance Cnv (DTH.MExp , r) (AUN.Exp TH.Name) where
  cnv (ee , r) = case ee of
    DTH.MLitE l         -> case l of
      TH.IntegerL  i    -> pure (AUN.Int  (fromInteger  i :: Word32))
      TH.RationalL i    -> pure (AUN.ConF (fromRational i :: Float))
      _                 -> fail "Not Supported!"
    DTH.MVarE n
      | n === 'fst          -> do vv1 <- newTHVar
                                  pure (AUN.Abs (vv1 ,
                                        AUN.Fst (AUN.Var vv1)))
      | n === 'snd          -> do vv1 <- newTHVar
                                  pure (AUN.Abs (vv1 ,
                                        AUN.Snd (AUN.Var vv1)))
      | n === 'save         -> do v1 <- newTHVar
                                  pure (AUN.Abs (v1 ,
                                       AUN.Mem (AUN.Var v1)))
      | n === 'lnArr        -> do v1 <- newTHVar
                                  pure (AUN.Abs (v1 ,
                                        AUN.Len (AUN.Var v1)))
      | n === 'ixArr        -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (AUN.Abs (v1 ,
                                     AUN.Abs (v2 ,
                                     AUN.Ind (AUN.Var v1)
                                             (AUN.Var v2))))
      | n === '(*)          -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (AUN.Abs (v1 ,
                                        AUN.Abs (v2 ,
                                        AUN.Mul (AUN.Var v1)
                                                (AUN.Var v2))))
      | n === '(+)          -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (AUN.Abs (v1 ,
                                        AUN.Abs (v2 ,
                                        AUN.Add (AUN.Var v1)
                                                (AUN.Var v2))))
      | n === '(-)          -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (AUN.Abs (v1 ,
                                        AUN.Abs (v2 ,
                                        AUN.Sub (AUN.Var v1)
                                                (AUN.Var v2))))
      | n === '(==)         -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (AUN.Abs (v1 ,
                                        AUN.Abs (v2 ,
                                        AUN.Eql (AUN.Var v1)
                                                (AUN.Var v2))))
      | n === '(<)          -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (AUN.Abs (v1 ,
                                        AUN.Abs (v2 ,
                                        AUN.Ltd (AUN.Var v1)
                                                (AUN.Var v2))))
      | n === 'mkArr        -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (AUN.Abs (v1 ,
                                        AUN.Abs (v2 ,
                                        AUN.Ary (AUN.Var v1)
                                                (AUN.Var v2))))
      | n === 'while        -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  v3 <- newTHVar
                                  pure (AUN.Abs (v1 ,
                                     AUN.Abs (v2 ,
                                     AUN.Abs (v3 ,
                                     AUN.Whl (AUN.Var v1) (AUN.Var v2)
                                             (AUN.Var v3)))))
      | n === 'fix          -> do v1 <- newTHVar
                                  pure (AUN.Abs (v1 ,
                                        AUN.Fix (AUN.Var v1)))
      | otherwise           -> pure (AUN.Var (stripNameSpace n))
    DTH.MConE n
      | n === 'True         -> pure (AUN.ConB True)
      | n === 'False        -> pure (AUN.ConB False)
      | n === 'Nothing      -> pure AUN.Non
      | n === 'Just         -> do v1 <- newTHVar
                                  pure (AUN.Abs (v1 ,
                                        AUN.Som (AUN.Var v1)))
      | n === '(,)          -> do vv1 <- newTHVar
                                  vv2 <- newTHVar
                                  pure (AUN.Abs (vv1 ,
                                        AUN.Abs (vv2 ,
                                        AUN.Tpl (AUN.Var vv1)
                                                (AUN.Var vv1))))
      | n === 'Vec          -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (AUN.Abs (v1 ,
                                        AUN.Abs (v2 ,
                                        AUN.AryV (AUN.Var v1)
                                                 (AUN.Var v2))))
      | n === '(:+)         -> do v1 <- newTHVar
                                  v2 <- newTHVar
                                  pure (AUN.Abs (v1 ,
                                        AUN.Abs (v2 ,
                                        AUN.Cmx (AUN.Var v1)
                                                (AUN.Var v2))))
      | otherwise       -> pure (AUN.Var (stripNameSpace n))
    DTH.MAppE (DTH.MAppE (DTH.MConE n) el) l
      | n === '(,)      -> AUN.Tpl  <$> cnvWth r el <*> cnvWth r l
      | n === 'Vec      -> AUN.AryV <$> cnvWth r el <*> cnvWth r l
      | n === '(:+)     -> AUN.Cmx  <$> cnvWth r el <*> cnvWth r l
    DTH.MAppE (DTH.MConE n) e
      | n === 'Just     -> AUN.Som  <$> cnvWth r e
      | n === 'Vec      -> do v1 <- newTHVar
                              e' <- cnvWth r e
                              pure (AUN.Abs (v1 ,
                                    AUN.AryV e' (AUN.Var v1)))
      | n === '(,)      -> do v1 <- newTHVar
                              e' <- cnvWth r e
                              pure (AUN.Abs (v1 ,
                                    AUN.Tpl e' (AUN.Var v1)))
      | n === '(:+)     -> do v1 <- newTHVar
                              e' <- cnvWth r e
                              pure (AUN.Abs (v1 ,
                                    AUN.Cmx e' (AUN.Var v1)))
    DTH.MAppE (DTH.MVarE n) e
      | n === 'fst      -> AUN.Fst  <$> cnvWth r e
      | n === 'snd      -> AUN.Snd  <$> cnvWth r e
      | n === 'save     -> AUN.Mem  <$> cnvWth r e
      | n === 'lnArr    -> AUN.Len  <$> cnvWth r e
      | n === 'ixArr    -> do v1 <- newTHVar
                              e' <- cnvWth r e
                              pure (AUN.Abs (v1 ,
                                    AUN.Ind e' (AUN.Var v1)))
      | n === '(*)      -> do v1 <- newTHVar
                              e' <- cnvWth r e
                              pure (AUN.Abs (v1 ,
                                    AUN.Mul e' (AUN.Var v1)))
      | n === '(+)      -> do v1 <- newTHVar
                              e' <- cnvWth r e
                              pure (AUN.Abs (v1 ,
                                    AUN.Add e' (AUN.Var v1)))
      | n === '(-)      -> do v1 <- newTHVar
                              e' <- cnvWth r e
                              pure (AUN.Abs (v1 ,
                                    AUN.Sub e' (AUN.Var v1)))
      | n === '(==)     -> do v1 <- newTHVar
                              e' <- cnvWth r e
                              pure (AUN.Abs (v1 ,
                                    AUN.Eql e' (AUN.Var v1)))
      | n === '(<)      -> do v1 <- newTHVar
                              e' <- cnvWth r e
                              pure (AUN.Abs (v1 ,
                                    AUN.Ltd e' (AUN.Var v1)))
      | n === 'mkArr    -> do v1 <- newTHVar
                              e' <- cnvWth r e
                              pure (AUN.Abs (v1 ,
                                    AUN.Ary e' (AUN.Var v1)))
      | n === 'while    -> do v1 <- newTHVar
                              v2 <- newTHVar
                              e' <- cnvWth r e
                              pure (AUN.Abs (v1 ,
                                    AUN.Abs (v2 ,
                                    AUN.Whl e' (AUN.Var v1)
                                               (AUN.Var v2))))
      | n === 'fix      -> AUN.Fix  <$> cnvWth r e
    DTH.MAppE (DTH.MAppE (DTH.MVarE n) el) er
      | n === 'ixArr    -> AUN.Ind  <$> cnvWth r el <*> cnvWth r er
      | n === '(*)      -> AUN.Mul  <$> cnvWth r el <*> cnvWth r er
      | n === '(+)      -> AUN.Add  <$> cnvWth r el <*> cnvWth r er
      | n === '(-)      -> AUN.Sub  <$> cnvWth r el <*> cnvWth r er
      | n === '(==)     -> AUN.Eql  <$> cnvWth r el <*> cnvWth r er
      | n === '(<)      -> AUN.Ltd  <$> cnvWth r el <*> cnvWth r er
      | n === 'mkArr    -> AUN.Ary  <$> cnvWth r el <*> cnvWth r er
      | n === 'while    -> do v1 <- newTHVar
                              el' <- cnvWth r el
                              er' <- cnvWth r er
                              pure (AUN.Abs (v1 ,
                                    AUN.Whl el' er' (AUN.Var v1)))
    DTH.MAppE (DTH.MAppE (DTH.MAppE (DTH.MVarE n) l1) l2) ei
      | n === 'while    -> AUN.Whl  <$> cnvWth r l1 <*> cnvWth r l2 <*> cnvWth r ei
    DTH.MAppE ef ea     -> AUN.App  <$> cnvWth r ef <*> cnvWth r ea
    DTH.MLamE x   eb    -> AUN.Abs  <$> cnvWth r (x , eb)
    DTH.MSigE e  t      -> AUN.Typ  <$> cnv  t <*> cnvWth r e
    DTH.MLetE x el eb   ->
        AUN.LeT  <$> cnvWth r el <*> cnvWth r (x , eb)
    DTH.MCaseE ec [(DTH.DConPa n [DTH.DVarPa xf , DTH.DVarPa xs],eb)]
         | n === '(,)    -> do v1 <- newTHVar
                               ec' <- cnvWth r ec
                               eb' <- cnvWth r eb
                               pure (AUN.LeT ec' (v1 ,
                                     AUN.LeT (AUN.Fst (AUN.Var v1))
                                     (xf ,
                                     AUN.LeT (AUN.Snd (AUN.Var v1))
                                     (xs , eb'))))
         | n === 'Vec    -> do v1 <- newTHVar
                               v2 <- newTHVar
                               ec' <- cnvWth r ec
                               eb' <- cnvWth r eb
                               pure (AUN.LeT ec' (v1 ,
                                     AUN.LeT (AUN.LenV (AUN.Var v1))
                                     (xf ,
                                     AUN.LeT (AUN.Abs (v2 ,
                                     AUN.IndV (AUN.Var v1)
                                              (AUN.Var v2)))
                                     (xs , eb'))))
    DTH.MCaseE ec [(DTH.DConPa n [] , el),
                   (DTH.DConPa m [] , er)]
         | n === 'False,
           m === 'True  -> AUN.Cnd  <$> cnvWth r ec <*> cnvWth r er <*> cnvWth r el
    DTH.MCaseE ec [(DTH.DConPa n [] , el),
                   (DTH.DConPa m [] , er)]
         | m === 'False,
           n === 'True  -> AUN.Cnd  <$> cnvWth r ec <*> cnvWth r el <*> cnvWth r er
    DTH.MCaseE ec [(DTH.DConPa nl [] , el),
                   (DTH.DConPa nr [DTH.DVarPa xr] , er)]
        | nl === 'Nothing ,
          nr === 'Just  -> AUN.May <$> cnvWth r ec <*> cnvWth r el <*>
                           cnvWth r (DTH.MLamE xr er)
    DTH.MCaseE ec [(DTH.DConPa nl [DTH.DVarPa xr] , el),
                   (DTH.DConPa nr [] , er)]
        | nr === 'Nothing ,
          nl === 'Just  -> AUN.May <$> cnvWth r ec <*> cnvWth r er <*>
                           cnvWth r (DTH.MLamE xr el)
    DTH.MCaseE _ _      -> fail "case expression form is not supported!"

instance Cnv ((TH.Name , DTH.MExp) , r) (TH.Name , AUN.Exp TH.Name) where
    cnv ((x , e) , r) = (,) <$> pure (stripNameSpace x) <*> cnvWth r e

newTHVar :: NamM ErrM TH.Name
newTHVar = do v1 <- newVar
              return (stripNameSpace (TH.mkName v1))

instance Cnv (TH.Exp , ()) (AUN.Exp TH.Name) where
  cnv (ee , _) =  do
    ee' :: DTH.MExp <- unQM (TH.runQ (DTH.desugar ee))
    cnv (ee' , ())

data QM a = QM {unQM :: StateT GHC.Types.Int ErrM a}

instance Applicative QM where
  pure = QM . pure
  f <*> g = QM (unQM f <*> unQM g)

instance Functor QM where
  fmap f = QM . fmap f . unQM

instance Monad QM where
  return       = QM . return
  (QM m) >>= f = QM (m >>= (unQM .f))


instance TH.Quasi QM where
  qNewName s          = QM (do n <- newVar
                               return (TH.mkName (n++s)))
  qReport b e         = QM (fail (if b
                                  then ("Error: " ++ e)
                                  else ("Warning: " ++ e)))
  qRecover m1 m2      = QM (StateT (\ s -> case runStateT (unQM m1) s of
                                             Lft _ -> runStateT (unQM m2) s
                                             x     -> x))
  qReify  n
    | n === 'False    = QM (return $(do {i <- TH.reify 'False; TH.lift i}))
    | n === 'True     = QM (return $(do {i <- TH.reify 'True; TH.lift i}))
    | n === ''Bool    = QM (return $(do {i <- TH.reify ''Bool; TH.lift i}))
    | n === 'Nothing  = QM (return $(do {i <- TH.reify 'Nothing; TH.lift i}))
    | n === 'Just     = QM (return $(do {i <- TH.reify 'Just; TH.lift i}))
    | n === ''Maybe   = QM (return $(do {i <- TH.reify ''Maybe; TH.lift i}))
    | n === '(,)      = QM (return $(do {i <- TH.reify '(,); TH.lift i}))
    | n === ''(,)     = QM (return $(do {i <- TH.reify ''(,); TH.lift i}))
    | otherwise       = QM (fail ("Not Supported for reification:\n"++ show n))

  qLookupName _ _     = QM (fail "Not Allowed!")
  qReifyInstances _ _ = QM (fail "Not Allowed!")
  qLocation           = QM (fail "Not Allowed!")
  qReifyRoles _       = QM (fail "Not Allowed!")
  qReifyAnnotations _ = QM (fail "Not Allowed!")
  qReifyModule _      = QM (fail "Not Allowed!")
  qAddDependentFile _ = QM (fail "Not Allowed!")
  qAddModFinalizer _  = QM (fail "Not Allowed!")
  qAddTopDecls _      = QM (fail "Not Allowed!")
  qRunIO _            = QM (fail "Not Allowed!")
  qPutQ _             = QM (fail "Not Allowed!")
  qGetQ               = QM (fail "Not Allowed!")
