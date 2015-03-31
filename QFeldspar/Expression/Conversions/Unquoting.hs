module QFeldspar.Expression.Conversions.Unquoting () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Type.ADT as TA
import qualified QFeldspar.Expression.ADTUntypedNamed as AUN
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Desugar as DTH
import qualified GHC.Types
import Language.Haskell.TH.Instances ()
import QFeldspar.Expression.Utils.TemplateHaskell
import QFeldspar.Conversion
import QFeldspar.Type.Conversion ()

instance Cnv (DTH.DExp , r) (AUN.Exp TH.Name) where
  cnv (ee , r) = let ?r = r in case ee of
    DTH.DLitE l         -> case l of
      TH.IntegerL  i    -> pure (AUN.Int  (fromInteger  i :: Int))
      TH.RationalL i    -> pure (AUN.ConF (fromRational i :: Flt))
      _                 -> fail "Not Supported!"
    DTH.DVarE n
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
      | otherwise           -> pure (AUN.Var (stripNameSpace n))
    DTH.DConE n
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
    DTH.DAppE (DTH.DAppE (DTH.DConE n) el) l
      | n === '(,)      -> AUN.Tpl  <$@> el <*@> l
      | n === 'Vec      -> AUN.AryV <$@> el <*@> l
      | n === '(:+)     -> AUN.Cmx  <$@> el <*@> l
    DTH.DAppE (DTH.DConE n) e
      | n === 'Just     -> AUN.Som  <$@> e
      | n === 'Vec      -> do v1 <- newTHVar
                              e' <- cnvImp e
                              pure (AUN.Abs (v1 ,
                                    AUN.AryV e' (AUN.Var v1)))
      | n === '(,)      -> do v1 <- newTHVar
                              e' <- cnvImp e
                              pure (AUN.Abs (v1 ,
                                    AUN.Tpl e' (AUN.Var v1)))
      | n === '(:+)     -> do v1 <- newTHVar
                              e' <- cnvImp e
                              pure (AUN.Abs (v1 ,
                                    AUN.Cmx e' (AUN.Var v1)))
    DTH.DAppE (DTH.DVarE n) e
      | n === 'fst      -> AUN.Fst  <$@> e
      | n === 'snd      -> AUN.Snd  <$@> e
      | n === 'save     -> AUN.Mem  <$@> e
      | n === 'lnArr    -> AUN.Len  <$@> e
      | n === 'ixArr    -> do v1 <- newTHVar
                              e' <- cnvImp e
                              pure (AUN.Abs (v1 ,
                                    AUN.Ind e' (AUN.Var v1)))
      | n === '(*)      -> do v1 <- newTHVar
                              e' <- cnvImp e
                              pure (AUN.Abs (v1 ,
                                    AUN.Mul e' (AUN.Var v1)))
      | n === '(+)      -> do v1 <- newTHVar
                              e' <- cnvImp e
                              pure (AUN.Abs (v1 ,
                                    AUN.Add e' (AUN.Var v1)))
      | n === '(-)      -> do v1 <- newTHVar
                              e' <- cnvImp e
                              pure (AUN.Abs (v1 ,
                                    AUN.Sub e' (AUN.Var v1)))
      | n === '(==)     -> do v1 <- newTHVar
                              e' <- cnvImp e
                              pure (AUN.Abs (v1 ,
                                    AUN.Eql e' (AUN.Var v1)))
      | n === '(<)      -> do v1 <- newTHVar
                              e' <- cnvImp e
                              pure (AUN.Abs (v1 ,
                                    AUN.Ltd e' (AUN.Var v1)))
      | n === 'mkArr    -> do v1 <- newTHVar
                              e' <- cnvImp e
                              pure (AUN.Abs (v1 ,
                                    AUN.Ary e' (AUN.Var v1)))
      | n === 'while    -> do v1 <- newTHVar
                              v2 <- newTHVar
                              e' <- cnvImp e
                              pure (AUN.Abs (v1 ,
                                    AUN.Abs (v2 ,
                                    AUN.Whl e' (AUN.Var v1)
                                               (AUN.Var v2))))
    DTH.DAppE (DTH.DAppE (DTH.DVarE n) el) er
      | n === 'ixArr    -> AUN.Ind  <$@> el <*@> er
      | n === '(*)      -> AUN.Mul  <$@> el <*@> er
      | n === '(+)      -> AUN.Add  <$@> el <*@> er
      | n === '(-)      -> AUN.Sub  <$@> el <*@> er
      | n === '(==)     -> AUN.Eql  <$@> el <*@> er
      | n === '(<)      -> AUN.Ltd  <$@> el <*@> er
      | n === 'mkArr    -> AUN.Ary  <$@> el <*@> er
      | n === 'while    -> do v1 <- newTHVar
                              el' <- cnvImp el
                              er' <- cnvImp er
                              pure (AUN.Abs (v1 ,
                                    AUN.Whl el' er' (AUN.Var v1)))
    DTH.DAppE (DTH.DAppE (DTH.DAppE (DTH.DVarE n) l1) l2) ei
      | n === 'while    -> AUN.Whl  <$@> l1 <*@> l2 <*@> ei
    DTH.DAppE ef ea     -> AUN.App  <$@> ef <*@> ea
    DTH.DLamE []  _     -> fail "Bad Syntax!"
    DTH.DLamE [x] eb    -> AUN.Abs  <$@> (x , eb)
    DTH.DLamE (x:xs) eb -> cnvImp (DTH.DLamE [x] (DTH.DLamE xs eb))
        -- simple but not a good idea
    DTH.DSigE e  t      -> AUN.Typ  <$@> t  <*@> e
    DTH.DLetE [] _      -> fail "Bad Syntax!"
    DTH.DLetE [DTH.DValD (DTH.DVarPa x) el] eb ->
        AUN.Let  <$@> el <*@> (x , eb)
    DTH.DLetE _  _      ->  fail "let bindings forms other than let x = M in N are not supported!"
    DTH.DCaseE ec [DTH.DMatch
      (DTH.DConPa n [DTH.DVarPa xf , DTH.DVarPa xs]) eb]
         | n === '(,)    -> do v1 <- newTHVar
                               ec' <- cnvImp ec
                               eb' <- cnvImp eb
                               pure (AUN.Let ec' (v1 ,
                                     AUN.Let (AUN.Fst (AUN.Var v1))
                                     (xf ,
                                     AUN.Let (AUN.Snd (AUN.Var v1))
                                     (xs , eb'))))
         | n === 'Vec    -> do v1 <- newTHVar
                               v2 <- newTHVar
                               ec' <- cnvImp ec
                               eb' <- cnvImp eb
                               pure (AUN.Let ec' (v1 ,
                                     AUN.Let (AUN.LenV (AUN.Var v1))
                                     (xf ,
                                     AUN.Let (AUN.Abs (v2 ,
                                     AUN.IndV (AUN.Var v1)
                                              (AUN.Var v2)))
                                     (xs , eb'))))
    DTH.DCaseE ec [DTH.DMatch (DTH.DConPa n []) el,
                   DTH.DMatch (DTH.DConPa m []) er]
         | n === 'False,
           m === 'True  -> AUN.Cnd  <$@> ec <*@> er <*@> el
    DTH.DCaseE ec [DTH.DMatch (DTH.DConPa n []) el,
                   DTH.DMatch (DTH.DConPa m []) er]
         | m === 'False,
           n === 'True  -> AUN.Cnd  <$@> ec <*@> el <*@> er
    DTH.DCaseE ec [DTH.DMatch (DTH.DConPa nl []) el,
                   DTH.DMatch (DTH.DConPa nr [DTH.DVarPa xr]) er]
        | nl === 'Nothing ,
          nr === 'Just  -> AUN.May <$@> ec <*@> el <*@>
                           (DTH.DLamE [xr] er)
    DTH.DCaseE ec [DTH.DMatch (DTH.DConPa nl [DTH.DVarPa xr]) el,
                   DTH.DMatch (DTH.DConPa nr []) er]
        | nr === 'Nothing ,
          nl === 'Just  -> AUN.May <$@> ec <*@> er <*@>
                           (DTH.DLamE [xr] el)
    DTH.DCaseE _ _      -> fail "case expression form is not supported!"
    DTH.DStaticE _      -> fail "Not supported!"

instance Cnv ((TH.Name , DTH.DExp) , r) (TH.Name , AUN.Exp TH.Name) where
    cnv ((x , e) , r) = let ?r = r
                        in (,) <$> pure (stripNameSpace x) <*@> e

instance Cnv (DTH.DType , r) TA.Typ where
  cnv (th , r) = let ?r = r in case th of
   DTH.DConT n
       | n == ''Word32                     -> pure TA.Int
       | n == ''Int                        -> pure TA.Int
       | n == ''Bol                        -> pure TA.Bol
       | n == ''Bool                       -> pure TA.Bol
       | n == ''Float                      -> pure TA.Flt
       | n == ''Flt                        -> pure TA.Flt
       | n == ''Cmx                        -> pure TA.Cmx
   DTH.DAppT (DTH.DAppT (DTH.DConT n) (DTH.DConT m)) a
       | n == ''Array && m == ''Word32     -> TA.Ary <$@> a
       | n == ''Array && m == ''Int        -> TA.Ary <$@> a
   DTH.DAppT (DTH.DAppT DTH.DArrowT   a) b -> TA.Arr <$@> a <*@> b
   DTH.DAppT (DTH.DAppT (DTH.DConT n) a) b
       | n == ''Arr                        -> TA.Arr <$@> a <*@> b
       | n == ''(,)                        -> TA.Tpl <$@> a <*@> b
       | n == ''Tpl                        -> TA.Tpl <$@> a <*@> b
   DTH.DAppT (DTH.DConT n) (DTH.DConT m)
       | n == ''Complex && m == ''Float    -> pure TA.Cmx
       | n == ''Complex && m == ''Flt      -> pure TA.Cmx
   DTH.DAppT (DTH.DConT n) a
       | n == ''Maybe                      -> TA.May <$@> a
       | n == ''May                        -> TA.May <$@> a
       | n == ''Ary                        -> TA.Ary <$@> a
       | n == ''Vec                        -> TA.Vec <$@> a
   _            -> fail ("Syntax not supported:\n" ++ show th)

-- not supported:
--           | DLitT TyLit
--           | DVarT Name
--           | DSigT DType DKind
--           | DForallT [DTyVarBndr] DCxt DType

newTHVar :: NamM ErrM TH.Name
newTHVar = do v1 <- newVar
              return (stripNameSpace (TH.mkName v1))

instance Cnv (TH.Exp , ()) (AUN.Exp TH.Name) where
  cnv (ee , _) =  do
    ee' :: DTH.DExp <- unQM (TH.runQ (DTH.desugar ee))
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
