module QFeldspar.Type.Conversion () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Type.ADT  as TFA
import qualified QFeldspar.Type.GADT as TFG
import qualified QFeldspar.Type.Herbrand      as HR
import qualified Language.Haskell.TH          as TH

import QFeldspar.Conversion

---------------------------------------------------------------------------------
--  Conversion from TFA.Typ
---------------------------------------------------------------------------------

instance Cnv (TFA.Typ) (ExsSin TFG.Typ) where
  cnv TFA.Wrd         = return (ExsSin TFG.Wrd)
  cnv TFA.Bol         = return (ExsSin TFG.Bol)
  cnv TFA.Flt         = return (ExsSin TFG.Flt)

  cnv (TFA.Arr ta tr) = do ExsSin ta' <- cnv ta
                           ExsSin tr' <- cnv tr
                           return (ExsSin (TFG.Arr ta' tr'))
  cnv (TFA.Tpl tf ts) = do ExsSin tf' <- cnv tf
                           ExsSin ts' <- cnv ts
                           return (ExsSin (TFG.Tpl tf' ts'))
  cnv (TFA.Ary t)     = do ExsSin t' <- cnv t
                           return (ExsSin (TFG.Ary t'))
  cnv (TFA.Vec t)     = do ExsSin t' <- cnv t
                           return (ExsSin (TFG.Vct t'))
  cnv TFA.Cmx         = return (ExsSin TFG.Cmx)
  cnv (TFA.May t)     = do ExsSin t' <- cnv t
                           return (ExsSin (TFG.May t'))

instance Cnv (TFA.Typ , r) (HR.Typ (HR.EnvFld '[])) where
  cnv (th , r) = case th of
    TFA.Wrd       -> pure HR.Wrd
    TFA.Bol       -> pure HR.Bol
    TFA.Flt       -> pure HR.Flt
    TFA.Arr ta tb -> HR.Arr <$> cnvWth r ta <*> cnvWth r tb
    TFA.Tpl tf ts -> HR.Tpl <$> cnvWth r tf <*> cnvWth r ts
    TFA.Ary ta    -> HR.Ary <$> cnvWth r ta
    TFA.Vec ta    -> HR.Vec <$> cnvWth r ta
    TFA.May ta    -> HR.May <$> cnvWth r ta
    TFA.Cmx       -> pure HR.Cmx

instance Cnv (TFA.Typ , r) TH.Type where
  cnv (th , r) = case th of
    TFA.Wrd       -> pure (TH.ConT ''Word32)
    TFA.Bol       -> pure (TH.ConT ''Bool)
    TFA.Flt       -> pure (TH.ConT ''Float)
    TFA.Arr ta tb -> do ta' <- cnvWth r ta
                        tb' <- cnvWth r tb
                        pure (TH.AppT (TH.AppT (TH.ConT ''(->)) ta') tb')
    TFA.Tpl tf ts -> do ta' <- cnvWth r tf
                        tb' <- cnvWth r ts
                        pure (TH.AppT (TH.AppT (TH.ConT ''(,)) ta') tb')
    TFA.Ary ta    -> TH.AppT (TH.ConT ''Ary) <$> cnvWth r ta
    TFA.Vec ta    -> TH.AppT (TH.ConT ''Vec) <$> cnvWth r ta
    TFA.May ta    -> TH.AppT (TH.ConT ''Maybe) <$> cnvWth r ta
    TFA.Cmx       -> pure (TH.AppT (TH.ConT ''Complex) (TH.ConT ''Float))

---------------------------------------------------------------------------------
--  Conversion from TFG.Typ
---------------------------------------------------------------------------------

instance Cnv (TFG.Typ a , r) TFA.Typ where
  cnv (tt , r) = case tt of
    TFG.Wrd       -> pure TFA.Wrd
    TFG.Bol       -> pure TFA.Bol
    TFG.Flt       -> pure TFA.Flt
    TFG.Arr ta tb -> TFA.Arr <$> cnvWth r ta <*> cnvWth r tb
    TFG.Tpl tf ts -> TFA.Tpl <$> cnvWth r tf <*> cnvWth r ts
    TFG.Ary ta    -> TFA.Ary <$> cnvWth r ta
    TFG.Vct ta    -> TFA.Vec <$> cnvWth r ta
    TFG.May ta    -> TFA.May <$> cnvWth r ta
    TFG.Cmx       -> pure TFA.Cmx


instance Cnv (TFG.Typ a , r) TH.Type where
  cnv (t , r) = do t' :: TFA.Typ <- cnv (t , r)
                   cnv (t' , r)

---------------------------------------------------------------------------------
--  Conversion from HR.Typ
---------------------------------------------------------------------------------

instance Cnv (HR.Typ (HR.EnvFld '[]) , r) TFA.Typ where
  cnv (th , r) = case th of
    HR.Wrd       -> pure TFA.Wrd
    HR.Bol       -> pure TFA.Bol
    HR.Flt       -> pure TFA.Flt
    HR.Arr ta tb -> TFA.Arr <$> cnvWth r ta <*> cnvWth r tb
    HR.Tpl tf ts -> TFA.Tpl <$> cnvWth r tf <*> cnvWth r ts
    HR.Ary t     -> TFA.Ary <$> cnvWth r t
    HR.Vec t     -> TFA.Vec <$> cnvWth r t
    HR.May t     -> TFA.May <$> cnvWth r t
    HR.Cmx       -> pure TFA.Cmx
    _            -> fail ("Type Error:\n" ++ show th)

instance ts ~ ts' => Cnv (TFG.Typ ts, r) (TFG.Typ ts') where
  cnv = pure . fst
{-
---------------------------------------------------------------------------------
--  Conversion from HR.Typ
---------------------------------------------------------------------------------

instance Cnv (TH.Type , r) TFA.Typ where
  cnv (th , r) = let ?r = r in case th of
   TH.ConT n
       | n == ''Word32                  -> pure TFA.Int
       | n == ''Int                     -> pure TFA.Int
       | n == ''Bol                     -> pure TFA.Bol
       | n == ''Bool                    -> pure TFA.Bol
       | n == ''Float                   -> pure TFA.Flt
       | n == ''Flt                     -> pure TFA.Flt
       | n == ''Cmx                     -> pure TFA.Cmx
   TH.AppT (TH.AppT (TH.ConT n) (TH.ConT m)) a
       | n == ''Array && m == ''Word32  -> TFA.Ary <$@> a
       | n == ''Array && m == ''Int     -> TFA.Ary <$@> a
   TH.AppT (TH.AppT TH.ArrowT a) b      -> TFA.Arr <$@> a <*@> b
   TH.AppT (TH.AppT (TH.TupleT 2) a) b  -> TFA.Tpl <$@> a <*@> b
   TH.AppT (TH.AppT (TH.ConT n)  a) b
       | n == ''Arr                     -> TFA.Arr <$@> a <*@> b
       | n == ''Tpl                     -> TFA.Tpl <$@> a <*@> b
   TH.AppT (TH.ConT n) (TH.ConT m)
       | n == ''Complex && m == ''Float -> pure TFA.Cmx
       | n == ''Complex && m == ''Flt   -> pure TFA.Cmx
   TH.AppT (TH.ConT n) a
       | n == ''Maybe                   -> TFA.May <$@> a
       | n == ''May                     -> TFA.May <$@> a
       | n == ''Ary                     -> TFA.Ary <$@> a
       | n == ''Vec                     -> TFA.Vec <$@> a
   _            -> fail ("Type Error:\n" ++ show th)
-}
