module QFeldspar.Expression.Conversion () where

import QFeldspar.MyPrelude

import QFeldspar.Conversion
import QFeldspar.Singleton
import QFeldspar.Nat.ADT
import qualified QFeldspar.Nat.GADT as NG
import qualified Language.Haskell.TH.Syntax as TH
import qualified QFeldspar.Expression.ADTUntypedNamed as FAUN
import qualified QFeldspar.Expression.ADTUntypedDebruijn as FAUD
import qualified QFeldspar.Expression.GADTTyped as FGTD
import qualified QFeldspar.Expression.GADTFirstOrder as FGFO
import qualified QFeldspar.Expression.GADTHigherOrder as FGHO
import qualified QFeldspar.Expression.MiniFeldspar as FMWS
import qualified QFeldspar.Type.ADT as TFA
import qualified QFeldspar.Type.GADT as TFG
import qualified QFeldspar.Environment.Plain as EP
import qualified QFeldspar.Environment.Scoped as ES
import qualified QFeldspar.Environment.Typed as ET
import QFeldspar.Variable.Conversion ()
import QFeldspar.Normalisation (nrm)
import QFeldspar.Expression.Utils.Reuse.GADTHigherOrder(onHOAS)
import QFeldspar.Environment.Conversion ()
import QFeldspar.Type.Conversion ()
import QFeldspar.Expression.Conversions.Unquoting ()
import QFeldspar.Expression.Conversions.NameResolution ()
import QFeldspar.Expression.Conversions.ScopeWithnessing ()
import QFeldspar.Expression.Conversions.TypeInference ()
import QFeldspar.Expression.Conversions.TypeWithnessing ()
import QFeldspar.Expression.Conversions.Lifting ()
import QFeldspar.Expression.Conversions.Normalisation ()
import QFeldspar.Expression.Conversions.EtaPrims(etaPrms)

-----------------------------------------------------------------------
-- Conversion from TH.TExp
-----------------------------------------------------------------------
instance (s ~ s' , a ~ a' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FMWS.Exp s' a')
         where
  cnv (e , s , v) = do e' :: FGHO.Exp s a <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , a ~ a' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FGHO.Exp s' a')
         where
  cnv (e , s , v) = do e' :: FGFO.Exp s '[] a <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , a ~ a' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FGFO.Exp s' '[] a')
         where
  cnv (e , s , v) = do e' :: FGTD.Exp n Zro TFA.Typ <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ n' , n ~ Len s , HasSin TFG.Typ a)  =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FGTD.Exp n' Zro TFA.Typ)
         where
  cnv (e , s , v) = do e' :: FAUD.Exp <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s , HasSin TFG.Typ a) =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TFG.Typ s , ES.Env n TH.Name)
             FAUD.Exp
         where
  cnv (e , s , v) = do e' :: FAUN.Exp TH.Name <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s) =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FAUN.Exp TH.Name)
         where
  cnv (e , s , v) = do e' :: TH.Exp <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s) =>
         Cnv (TH.Q (TH.TExp a) , es , en)
             TH.Exp
         where
  cnv (e , _ , _) = lift (TH.runQ (TH.unTypeQ e))

instance (a ~ a' , n ~ Len s) =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (TH.Q (TH.TExp a'))
         where
  cnv (e , _ , _ ) = pure e

-----------------------------------------------------------------------
-- Conversion from TH.Exp
-----------------------------------------------------------------------
instance (s ~ s' , a ~ a' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (TH.Exp , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FMWS.Exp s' a')
         where
  cnv (e , s , v) = do e' :: FGHO.Exp s a' <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (TH.Exp , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FGHO.Exp s' a')
         where
  cnv (e , s , v) = do e' :: FGFO.Exp s '[] a' <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (TH.Exp , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FGFO.Exp s' '[] a')
         where
  cnv (e , s , v) = do e' :: FGTD.Exp n Zro TFA.Typ <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ n' , n ~ Len s)  =>
         Cnv (TH.Exp , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FGTD.Exp n' Zro TFA.Typ)
         where
  cnv (e , s , v) = do e' :: FAUD.Exp <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s) =>
         Cnv (TH.Exp , ET.Env TFG.Typ s , ES.Env n TH.Name)
             FAUD.Exp
         where
  cnv (e , s , v) = do e' :: FAUN.Exp TH.Name <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s) =>
         Cnv (TH.Exp , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FAUN.Exp TH.Name)
         where
  cnv (e , s , g) = do e' :: FAUN.Exp TH.Name <- cnv (e , ())
                       etaPrms s g e'

instance Cnv (TH.Exp , es , en)
             TH.Exp
         where
  cnv (e , _ , _) = pure e

-----------------------------------------------------------------------
-- Conversion from FAUN
-----------------------------------------------------------------------
instance (s ~ s' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (FAUN.Exp TH.Name , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FMWS.Exp s' a')
         where
  cnv (e , s , v) = do e' :: FGHO.Exp s a' <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , t ~ t' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (FAUN.Exp TH.Name , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FGHO.Exp s' a')
         where
  cnv (e , s , v) = do e' :: FGFO.Exp s' '[] a' <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (FAUN.Exp TH.Name , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FGFO.Exp s' '[] a')
         where
  cnv (e , s , v) = do e' :: FGTD.Exp n Zro TFA.Typ <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s)  =>
         Cnv (FAUN.Exp TH.Name , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FGTD.Exp n Zro TFA.Typ)
         where
  cnv (e , s , v) = do e' :: FAUD.Exp <- cnv (e , s , v)
                       cnv (e' , s , v)

instance Cnv (FAUN.Exp TH.Name , es , ES.Env n TH.Name)
             FAUD.Exp
         where
  cnv (e , _ , vs) = do vs' :: EP.Env TH.Name <- cnv (vs , ())
                        cnv (e , (vs' , [] :: EP.Env TH.Name))

instance Cnv (FAUN.Exp TH.Name , es , en)
             (FAUN.Exp TH.Name)
         where
  cnv (e , _ , _ ) = pure e

-----------------------------------------------------------------------
-- Conversion from FAUD
-----------------------------------------------------------------------
instance (s ~ s' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (FAUD.Exp , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FMWS.Exp s' a')
         where
  cnv (e , s , v) = do e' :: FGHO.Exp s a' <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (FAUD.Exp , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FGHO.Exp s' a')
         where
  cnv (e , s , v) = do e' :: FGFO.Exp s '[] a' <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (FAUD.Exp , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FGFO.Exp s' '[] a')
         where
  cnv (e , s , v) = do e' :: FGTD.Exp n Zro TFA.Typ <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n' ~ Len s) =>
         Cnv (FAUD.Exp , ET.Env TFG.Typ s , en)
             (FGTD.Exp n' Zro TFA.Typ)
         where
  cnv (e , s , _) = do e' :: FGTD.Exp n' Zro (Maybe TFA.Typ) <- cnv (e , (ET.len s , NG.Zro))
                       s' :: ES.Env   n' TFA.Typ <- cnv (s , ())
                       cnv (e' , (s', ES.Emp :: ES.Env Zro TFA.Typ))

instance Cnv (FAUD.Exp , es , en)
             FAUD.Exp
         where
  cnv (e , _ , _ ) = pure e

-----------------------------------------------------------------------
-- Conversion from FGTD
-----------------------------------------------------------------------
instance (s ~ s' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (FGTD.Exp n Zro TFA.Typ , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FMWS.Exp s' a')
         where
  cnv (e , s , v) = do e' :: FGHO.Exp s a' <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (FGTD.Exp n Zro TFA.Typ , ET.Env TFG.Typ s , ES.Env n TH.Name)
             (FGHO.Exp s' a')
         where
  cnv (e , s , v) = do e' :: FGFO.Exp s '[] t <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (FGTD.Exp n Zro TFA.Typ , ET.Env TFG.Typ s , en)
             (FGFO.Exp s' '[] a')
         where
  cnv (e , s , _) = cnv (e , (s , ET.Emp :: ET.Env TFG.Typ '[]))

instance (n ~ n') =>
         Cnv (FGTD.Exp n  Zro TFA.Typ , es , en)
             (FGTD.Exp n' Zro TFA.Typ)
         where
  cnv (e , _ , _) = return e

-----------------------------------------------------------------------
-- Conversion from FGFO
-----------------------------------------------------------------------
instance (s ~ s' , a ~ a' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (FGFO.Exp s '[] a , ET.Env TFG.Typ s , en)
             (FMWS.Exp s' a')
         where
  cnv (e , s , _) = do e' :: FGHO.Exp s a <- cnv (e , s , ())
                       cnv (e' , s , ())

instance (s ~ s' , a ~ a' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (FGFO.Exp s '[] a , ET.Env TFG.Typ s , en)
             (FGHO.Exp s' a')
         where
  cnv (e , s , _) = cnv ({- nrm -} e , s)

instance (s ~ s' , a ~ a' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (FGFO.Exp s  '[] a , es , en)
             (FGFO.Exp s' '[] a')
         where
  cnv (e , _ , _) = pure e

-----------------------------------------------------------------------
-- Conversion from FGHO
-----------------------------------------------------------------------
instance (s ~ s' , a ~ a' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (FGHO.Exp s  a , ET.Env TFG.Typ s , en)
             (FMWS.Exp s' a')
         where
  cnv (e , s , _) = case getPrfHasSin s of
    PrfHasSin -> cnv ({-nrm (eta e)-} onHOAS nrm e , ())

instance (s ~ s' , a ~ a' , HasSin TFG.Typ a') =>
         Cnv (FGHO.Exp s  a , es , en)
             (FGHO.Exp s' a')
         where
  cnv (e , _ , _) = pure e

instance (s ~ s' , a ~ a' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (FGHO.Exp s  a , ET.Env TFG.Typ s , en)
             (FGFO.Exp s' '[] a')
         where
  cnv (e , s , _) = cnv (e , s)

-----------------------------------------------------------------------
-- Conversion from FMWS
-----------------------------------------------------------------------
instance (s ~ s' , a ~ a' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (FMWS.Exp s  a , es , en)
             (FMWS.Exp s' a')
         where
  cnv (e , _ , _) = pure e

instance (s ~ s' , a ~ a' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (FMWS.Exp s  a , es , en)
             (FGHO.Exp s' a')
         where
  cnv (e , _ , _) = cnv (e , ())

instance (s ~ s' , a ~ a' , n ~ Len s , HasSin TFG.Typ a') =>
         Cnv (FMWS.Exp s  a , ET.Env TFG.Typ s , en)
             (FGFO.Exp s' '[] a')
         where
  cnv (e , s , _) = do e' :: FGHO.Exp s a <- cnv (e , ())
                       cnv (e' , s)
