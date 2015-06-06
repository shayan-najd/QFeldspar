module QFeldspar.Type.Conversion () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Type.ADT  as TA
import qualified QFeldspar.Type.GADT as TG
import qualified QFeldspar.Type.Herbrand      as HR
import qualified Language.Haskell.TH          as TH

import QFeldspar.Conversion

---------------------------------------------------------------------------------
--  Conversion from TA.Typ
---------------------------------------------------------------------------------

instance Cnv (TA.Typ) (ExsSin TG.Typ) where
  cnv TA.Wrd         = return (ExsSin TG.Wrd)
  cnv TA.Bol         = return (ExsSin TG.Bol)
  cnv TA.Flt         = return (ExsSin TG.Flt)

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
  cnv TA.Cmx         = return (ExsSin TG.Cmx)
  cnv (TA.May t)     = do ExsSin t' <- cnv t
                          return (ExsSin (TG.May t'))

instance Cnv (TA.Typ , r)  (ExsSin TG.Typ) where
  cnv (t , _) = cnv t

instance Cnv (TA.Typ , r) (HR.Typ (HR.EnvFld '[])) where
  cnv (th , r) = case th of
    TA.Wrd       -> pure HR.Wrd
    TA.Bol       -> pure HR.Bol
    TA.Flt       -> pure HR.Flt
    TA.Arr ta tb -> HR.Arr <$> cnvWth r ta <*> cnvWth r tb
    TA.Tpl tf ts -> HR.Tpl <$> cnvWth r tf <*> cnvWth r ts
    TA.Ary ta    -> HR.Ary <$> cnvWth r ta
    TA.Vec ta    -> HR.Vec <$> cnvWth r ta
    TA.May ta    -> HR.May <$> cnvWth r ta
    TA.Cmx       -> pure HR.Cmx

instance Cnv (TA.Typ , r) TH.Type where
  cnv (th , r) = case th of
    TA.Wrd       -> pure (TH.ConT ''Word32)
    TA.Bol       -> pure (TH.ConT ''Bool)
    TA.Flt       -> pure (TH.ConT ''Float)
    TA.Arr ta tb -> do ta' <- cnvWth r ta
                       tb' <- cnvWth r tb
                       pure (TH.AppT (TH.AppT (TH.ConT ''(->)) ta') tb')
    TA.Tpl tf ts -> do ta' <- cnvWth r tf
                       tb' <- cnvWth r ts
                       pure (TH.AppT (TH.AppT (TH.ConT ''(,)) ta') tb')
    TA.Ary ta    -> TH.AppT (TH.ConT ''Ary) <$> cnvWth r ta
    TA.Vec ta    -> TH.AppT (TH.ConT ''Vec) <$> cnvWth r ta
    TA.May ta    -> TH.AppT (TH.ConT ''Maybe) <$> cnvWth r ta
    TA.Cmx       -> pure (TH.AppT (TH.ConT ''Complex) (TH.ConT ''Float))

---------------------------------------------------------------------------------
--  Conversion from TG.Typ
---------------------------------------------------------------------------------

instance Cnv (TG.Typ a , r) TA.Typ where
  cnv (tt , r) = case tt of
    TG.Wrd       -> pure TA.Wrd
    TG.Bol       -> pure TA.Bol
    TG.Flt       -> pure TA.Flt
    TG.Arr ta tb -> TA.Arr <$> cnvWth r ta <*> cnvWth r tb
    TG.Tpl tf ts -> TA.Tpl <$> cnvWth r tf <*> cnvWth r ts
    TG.Ary ta    -> TA.Ary <$> cnvWth r ta
    TG.Vct ta    -> TA.Vec <$> cnvWth r ta
    TG.May ta    -> TA.May <$> cnvWth r ta
    TG.Cmx       -> pure TA.Cmx


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
    HR.Arr ta tb -> TA.Arr <$> cnvWth r ta <*> cnvWth r tb
    HR.Tpl tf ts -> TA.Tpl <$> cnvWth r tf <*> cnvWth r ts
    HR.Ary t     -> TA.Ary <$> cnvWth r t
    HR.Vec t     -> TA.Vec <$> cnvWth r t
    HR.May t     -> TA.May <$> cnvWth r t
    HR.Cmx       -> pure TA.Cmx
    _            -> fail ("Type Error:\n" ++ show th)

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
