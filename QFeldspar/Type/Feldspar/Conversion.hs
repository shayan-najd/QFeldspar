module QFeldspar.Type.Feldspar.Conversion () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Type.Feldspar.ADT  as TFA
import qualified QFeldspar.Type.Feldspar.GADT as TFG
import qualified QFeldspar.Type.Herbrand      as HR
import qualified Language.Haskell.TH          as TH

import QFeldspar.Conversion

---------------------------------------------------------------------------------
--  Conversion from TFA.Typ
---------------------------------------------------------------------------------

instance Cnv (TFA.Typ) (ExsSin TFG.Typ) where
  cnv TFA.Int         = return (ExsSin TFG.Int)
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
  cnv (th , r) = let ?r = r in case th of
    TFA.Int       -> pure HR.Int
    TFA.Bol       -> pure HR.Bol
    TFA.Flt       -> pure HR.Flt
    TFA.Arr ta tb -> HR.Arr <$@> ta <*@> tb
    TFA.Tpl tf ts -> HR.Tpl <$@> tf <*@> ts
    TFA.Ary ta    -> HR.Ary <$@> ta
    TFA.Vec ta    -> HR.Vec <$@> ta
    TFA.May ta    -> HR.May <$@> ta
    TFA.Cmx       -> pure HR.Cmx

instance Cnv (TFA.Typ , r) TH.Type where
  cnv (th , r) = let ?r = r in case th of
    TFA.Int       -> pure (TH.ConT ''Int)
    TFA.Bol       -> pure (TH.ConT ''Bol)
    TFA.Flt       -> pure (TH.ConT ''Flt)
    TFA.Arr ta tb -> do ta' <- cnvImp ta
                        tb' <- cnvImp tb
                        pure (TH.AppT (TH.AppT (TH.ConT ''Arr) ta') tb')
    TFA.Tpl tf ts -> do ta' <- cnvImp tf
                        tb' <- cnvImp ts
                        pure (TH.AppT (TH.AppT (TH.ConT ''Tpl) ta') tb')
    TFA.Ary ta    -> TH.AppT (TH.ConT ''Ary) <$@> ta
    TFA.Vec ta    -> TH.AppT (TH.ConT ''Vec) <$@> ta
    TFA.May ta    -> TH.AppT (TH.ConT ''May) <$@> ta
    TFA.Cmx       -> pure (TH.ConT ''Cmx)

---------------------------------------------------------------------------------
--  Conversion from TFG.Typ
---------------------------------------------------------------------------------

instance Cnv (TFG.Typ a , r) TFA.Typ where
  cnv (tt , r) = let ?r = r in case tt of
    TFG.Int       -> pure TFA.Int
    TFG.Bol       -> pure TFA.Bol
    TFG.Flt       -> pure TFA.Flt
    TFG.Arr ta tb -> TFA.Arr <$@> ta <*@> tb
    TFG.Tpl tf ts -> TFA.Tpl <$@> tf <*@> ts
    TFG.Ary ta    -> TFA.Ary <$@> ta
    TFG.Vct ta    -> TFA.Vec <$@> ta
    TFG.May ta    -> TFA.May <$@> ta
    TFG.Cmx       -> pure TFA.Cmx


instance Cnv (TFG.Typ a , r) TH.Type where
  cnv (t , r) = do t' :: TFA.Typ <- cnv (t , r)
                   cnv (t' , r)

---------------------------------------------------------------------------------
--  Conversion from HR.Typ
---------------------------------------------------------------------------------

instance Cnv (HR.Typ (HR.EnvFld '[]) , r) TFA.Typ where
  cnv (th , r) = let ?r = r in case th of
    HR.Int       -> pure TFA.Int
    HR.Bol       -> pure TFA.Bol
    HR.Flt       -> pure TFA.Flt
    HR.Arr ta tb -> TFA.Arr <$@> ta <*@> tb
    HR.Tpl tf ts -> TFA.Tpl <$@> tf <*@> ts
    HR.Ary t     -> TFA.Ary <$@> t
    HR.Vec t     -> TFA.Vec <$@> t
    HR.May t     -> TFA.May <$@> t
    HR.Cmx       -> pure TFA.Cmx
    _            -> fail ("Type Error:\n" ++ show th)

instance ts ~ ts' => Cnv (TFG.Typ ts, r) (TFG.Typ ts') where
  cnv = pure . fst

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
