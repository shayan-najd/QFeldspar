module QFeldspar.Type.Conversion () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Type.ADT  as TA
import qualified QFeldspar.Type.GADT as TG
import qualified QFeldspar.Type.Herbrand      as HR
import qualified Language.Haskell.TH          as TH
import qualified Language.Haskell.TH.Desugar as  DTH
import QFeldspar.Nat.Conversion()
import qualified Data.Set as Set
import qualified QFeldspar.Environment.Map as EM
import QFeldspar.Conversion
import QFeldspar.Nat.ADT
import GHC.Real

---------------------------------------------------------------------------------
--  Conversion from TA.Typ
---------------------------------------------------------------------------------

instance Cnv (TA.Typ) (ExsSin TG.Typ) where
  cnv TA.Wrd         = return (ExsSin TG.Wrd)
  cnv TA.Bol         = return (ExsSin TG.Bol)
  cnv TA.Flt         = return (ExsSin TG.Flt)
  cnv TA.Cmx         = return (ExsSin TG.Cmx)
  cnv TA.Int         = return (ExsSin TG.Int)
  cnv TA.Rat         = return (ExsSin TG.Rat)
  cnv TA.Chr         = return (ExsSin TG.Chr)
  cnv TA.Str         = return (ExsSin TG.Str)
  cnv (TA.Arr ta tr) = do ExsSin ta' <- cnv ta
                          ExsSin tr' <- cnv tr
                          return (ExsSin (TG.Arr ta' tr'))
  cnv (TA.Tpl tf ts) = do ExsSin tf' <- cnv tf
                          ExsSin ts' <- cnv ts
                          return (ExsSin (TG.Tpl tf' ts'))
  cnv (TA.Ary t)     = do ExsSin t' <- cnv t
                          return (ExsSin (TG.Ary t'))
  cnv (TA.Vec t)     = do ExsSin t' <- cnv t
                          return (ExsSin (TG.Vct t'))
  cnv (TA.May t)     = do ExsSin t' <- cnv t
                          return (ExsSin (TG.May t'))
  cnv (TA.TVr n)     = do ExsSin n' <- cnv (n , ())
                          return (ExsSin (TG.TVr n'))

instance Cnv (TA.Typ , r)  (ExsSin TG.Typ) where
  cnv (t , _) = cnv t

instance Cnv (TA.Typ , r) (HR.Typ (HR.EnvFld '[])) where
  cnv (th , r) = case th of
    TA.Wrd       -> pure HR.Wrd
    TA.Bol       -> pure HR.Bol
    TA.Flt       -> pure HR.Flt
    TA.Cmx       -> pure HR.Cmx
    TA.Int       -> pure HR.Int
    TA.Rat       -> pure HR.Rat
    TA.Chr       -> pure HR.Chr
    TA.Str       -> pure HR.Str
    TA.Arr ta tb -> HR.Arr <$> cnvWth r ta <*> cnvWth r tb
    TA.Tpl tf ts -> HR.Tpl <$> cnvWth r tf <*> cnvWth r ts
    TA.Ary ta    -> HR.Ary <$> cnvWth r ta
    TA.Vec ta    -> HR.Vec <$> cnvWth r ta
    TA.May ta    -> HR.May <$> cnvWth r ta
    TA.TVr n     -> pure (HR.Mta n)

instance Cnv (TA.Typ , r) TH.Type where
  cnv (th , r) = case th of
    TA.Wrd       -> pure (TH.ConT ''Word32)
    TA.Bol       -> pure (TH.ConT ''Bool)
    TA.Flt       -> pure (TH.ConT ''Float)
    TA.Cmx       -> pure (TH.AppT (TH.ConT ''Complex) (TH.ConT ''Float))
    TA.Int       -> pure (TH.ConT ''Integer)
    TA.Rat       -> pure (TH.ConT ''Rational)
    TA.Chr       -> pure (TH.ConT ''Char)
    TA.Str       -> pure (TH.ConT ''String)
    TA.Arr ta tb -> do ta' <- cnvWth r ta
                       tb' <- cnvWth r tb
                       pure (TH.AppT (TH.AppT (TH.ConT ''(->)) ta') tb')
    TA.Tpl tf ts -> do ta' <- cnvWth r tf
                       tb' <- cnvWth r ts
                       pure (TH.AppT (TH.AppT (TH.ConT ''(,)) ta') tb')
    TA.Ary ta    -> TH.AppT (TH.ConT ''Ary) <$> cnvWth r ta
    TA.Vec ta    -> TH.AppT (TH.ConT ''Vec) <$> cnvWth r ta
    TA.May ta    -> TH.AppT (TH.ConT ''Maybe) <$> cnvWth r ta
    TA.TVr n     -> pure (TH.VarT (TH.mkName (show n)))

---------------------------------------------------------------------------------
--  Conversion from TG.Typ
---------------------------------------------------------------------------------

instance Cnv (TG.Typ a , r) TA.Typ where
  cnv (tt , r) = case tt of
    TG.Wrd       -> pure TA.Wrd
    TG.Bol       -> pure TA.Bol
    TG.Flt       -> pure TA.Flt
    TG.Cmx       -> pure TA.Cmx
    TG.Int       -> pure TA.Int
    TG.Rat       -> pure TA.Rat
    TG.Chr       -> pure TA.Chr
    TG.Str       -> pure TA.Str
    TG.Arr ta tb -> TA.Arr <$> cnvWth r ta <*> cnvWth r tb
    TG.Tpl tf ts -> TA.Tpl <$> cnvWth r tf <*> cnvWth r ts
    TG.Ary ta    -> TA.Ary <$> cnvWth r ta
    TG.Vct ta    -> TA.Vec <$> cnvWth r ta
    TG.May ta    -> TA.May <$> cnvWth r ta
    TG.TVr n     -> TA.TVr <$> cnvWth r n

instance Cnv (TG.Typ a , r) TH.Type where
  cnv (t , r) = do t' :: TA.Typ <- cnv (t , r)
                   cnv (t' , r)

---------------------------------------------------------------------------------
--  Conversion from HR.Typ
---------------------------------------------------------------------------------

instance Cnv (HR.Typ (HR.EnvFld '[]) , r) TA.Typ where
  cnv (th , r) = case th of
    HR.Wrd       -> pure TA.Wrd
    HR.Bol       -> pure TA.Bol
    HR.Flt       -> pure TA.Flt
    HR.Cmx       -> pure TA.Cmx
    HR.Int       -> pure TA.Int
    HR.Rat       -> pure TA.Rat
    HR.Chr       -> pure TA.Chr
    HR.Str       -> pure TA.Str
    HR.Arr ta tb -> TA.Arr <$> cnvWth r ta <*> cnvWth r tb
    HR.Tpl tf ts -> TA.Tpl <$> cnvWth r tf <*> cnvWth r ts
    HR.Ary t     -> TA.Ary <$> cnvWth r t
    HR.Vec t     -> TA.Vec <$> cnvWth r t
    HR.May t     -> TA.May <$> cnvWth r t
    _            -> impossible

instance ts ~ ts' => Cnv (TG.Typ ts, r) (TG.Typ ts') where
  cnv = pure . fst
{-
---------------------------------------------------------------------------------
--  Conversion from HR.Typ
---------------------------------------------------------------------------------

instance Cnv (TH.Type , r) TA.Typ where
  cnv (th , r) = let ?r = r in case th of
   TH.ConT n
       | n == ''Word32                  -> pure TA.Int
       | n == ''Int                     -> pure TA.Int
       | n == ''Bol                     -> pure TA.Bol
       | n == ''Bool                    -> pure TA.Bol
       | n == ''Float                   -> pure TA.Flt
       | n == ''Flt                     -> pure TA.Flt
       | n == ''Cmx                     -> pure TA.Cmx
   TH.AppT (TH.AppT (TH.ConT n) (TH.ConT m)) a
       | n == ''Array && m == ''Word32  -> TA.Ary <$@> a
       | n == ''Array && m == ''Int     -> TA.Ary <$@> a
   TH.AppT (TH.AppT TH.ArrowT a) b      -> TA.Arr <$@> a <*@> b
   TH.AppT (TH.AppT (TH.TupleT 2) a) b  -> TA.Tpl <$@> a <*@> b
   TH.AppT (TH.AppT (TH.ConT n)  a) b
       | n == ''Arr                     -> TA.Arr <$@> a <*@> b
       | n == ''Tpl                     -> TA.Tpl <$@> a <*@> b
   TH.AppT (TH.ConT n) (TH.ConT m)
       | n == ''Complex && m == ''Float -> pure TA.Cmx
       | n == ''Complex && m == ''Flt   -> pure TA.Cmx
   TH.AppT (TH.ConT n) a
       | n == ''Maybe                   -> TA.May <$@> a
       | n == ''May                     -> TA.May <$@> a
       | n == ''Ary                     -> TA.Ary <$@> a
       | n == ''Vec                     -> TA.Vec <$@> a
   _            -> fail ("Type Error:\n" ++ show th)
-}

instance Cnv DTH.DType TA.Typ where
  cnv t = cnvWth (zip (Set.toList (freeVarsT t)) [Zro ..]) t

instance Cnv (DTH.DType , EM.Env TH.Name Nat) TA.Typ where
  cnv (th , r) = case th of
   DTH.DConT n
       | n == ''Word32                     -> pure TA.Wrd
       | n == ''Bool                       -> pure TA.Bol
       | n == ''Float                      -> pure TA.Flt
       | n == ''Integer                    -> pure TA.Int
       | n == ''Rational                   -> pure TA.Rat
       | n == ''Char                       -> pure TA.Chr
       | n == ''String                     -> pure TA.Str
   DTH.DAppT (DTH.DConT n) (DTH.DConT m)
       | n == ''Complex && m == ''Float    -> pure TA.Cmx
       | n == ''Ratio   && m == ''Integer  -> pure TA.Rat
       | n == ''[]      && m == ''Char     -> pure TA.Str
   DTH.DAppT (DTH.DAppT (DTH.DConT n) (DTH.DConT m)) a
       | n == ''Array && m == ''Word32     -> TA.Ary <$> cnvWth r a
   DTH.DAppT (DTH.DAppT DTH.DArrowT   a) b -> TA.Arr <$> cnvWth r a <*> cnvWth r b
   DTH.DAppT (DTH.DAppT (DTH.DConT n) a) b
       | n == ''(->)                       -> TA.Arr <$> cnvWth r a <*> cnvWth r b
       | n == ''(,)                        -> TA.Tpl <$> cnvWth r a <*> cnvWth r b
   DTH.DAppT (DTH.DConT n) a
       | n == ''Maybe                      -> TA.May <$> cnvWth r a
       | n == ''Ary                        -> TA.Ary <$> cnvWth r a
       | n == ''Vec                        -> TA.Vec <$> cnvWth r a
   DTH.DVarT n                             -> TA.TVr <$> EM.get n r
   _            -> fail ("Syntax not supported:\n" ++ show th)

-- not supported:
--           | DLitT TyLit
--           | DVarT Name
--           | DSigT DType DKind
--           | DForallT [DTyVarBndr] DCxt DType


freeVarsT :: DTH.DType -> Set.Set TH.Name
freeVarsT ee = case ee of
  DTH.DAppT c b  -> freeVarsT c `Set.union` freeVarsT b
  DTH.DSigT a  _ -> freeVarsT a
  DTH.DVarT x    -> Set.singleton x
  DTH.DConT _    -> Set.empty
  DTH.DArrowT    -> Set.empty
  DTH.DLitT _    -> Set.empty
  _              -> badUse "freeVarsT"
