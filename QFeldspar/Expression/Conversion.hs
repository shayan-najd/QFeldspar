module QFeldspar.Expression.Conversion () where

import QFeldspar.MyPrelude

import QFeldspar.Conversion
import QFeldspar.Singleton
import QFeldspar.Nat.ADT
import qualified QFeldspar.Nat.GADT as NG
import qualified Language.Haskell.TH.Syntax as TH
import qualified QFeldspar.Expression.ADTUntypedNamed as AUN
import qualified QFeldspar.Expression.ADTUntypedDebruijn as AUD
import qualified QFeldspar.Expression.GADTTyped as GTD
import qualified QFeldspar.Expression.GADTFirstOrder as GFO
import qualified QFeldspar.Expression.GADTHigherOrder as GHO
import qualified QFeldspar.Expression.MiniFeldspar as MFS
import qualified QFeldspar.Type.ADT as TA
import qualified QFeldspar.Type.GADT as TG
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
instance (s ~ s' , a ~ a' , n ~ Len s , TG.Type a') =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TG.Typ s , ES.Env n TH.Name)
             (MFS.Exp s' a')
         where
  cnv (e , s , v) = do e' :: GHO.Exp s a <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , a ~ a' , n ~ Len s , TG.Type a') =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GHO.Exp s' a')
         where
  cnv (e , s , v) = do e' :: GFO.Exp s '[] a <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , a ~ a' , n ~ Len s , TG.Type a') =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GFO.Exp s' '[] a')
         where
  cnv (e , s , v) = do e' :: GTD.Exp n 'Zro TA.Typ <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ n' , n ~ Len s , TG.Type a)  =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GTD.Exp n' 'Zro TA.Typ)
         where
  cnv (e , s , v) = do e' :: AUD.Exp <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s , TG.Type a) =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TG.Typ s , ES.Env n TH.Name)
             AUD.Exp
         where
  cnv (e , s , v) = do e' :: AUN.Exp TH.Name <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s) =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TG.Typ s , ES.Env n TH.Name)
             (AUN.Exp TH.Name)
         where
  cnv (e , s , v) = do e' :: TH.Exp <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s) =>
         Cnv (TH.Q (TH.TExp a) , es , en)
             TH.Exp
         where
  cnv (e , _ , _) = lift (TH.runQ (TH.unTypeQ e))

instance (a ~ a' , n ~ Len s) =>
         Cnv (TH.Q (TH.TExp a) , ET.Env TG.Typ s , ES.Env n TH.Name)
             (TH.Q (TH.TExp a'))
         where
  cnv (e , _ , _ ) = pure e

-----------------------------------------------------------------------
-- Conversion from TH.Exp
-----------------------------------------------------------------------
instance (s ~ s' , a ~ a' , n ~ Len s , TG.Type a') =>
         Cnv (TH.Exp , ET.Env TG.Typ s , ES.Env n TH.Name)
             (MFS.Exp s' a')
         where
  cnv (e , s , v) = do e' :: GHO.Exp s a' <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , TG.Type a') =>
         Cnv (TH.Exp , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GHO.Exp s' a')
         where
  cnv (e , s , v) = do e' :: GFO.Exp s '[] a' <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , TG.Type a') =>
         Cnv (TH.Exp , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GFO.Exp s' '[] a')
         where
  cnv (e , s , v) = do e' :: GTD.Exp n 'Zro TA.Typ <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ n' , n ~ Len s)  =>
         Cnv (TH.Exp , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GTD.Exp n' 'Zro TA.Typ)
         where
  cnv (e , s , v) = do e' :: AUD.Exp <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s) =>
         Cnv (TH.Exp , ET.Env TG.Typ s , ES.Env n TH.Name)
             AUD.Exp
         where
  cnv (e , s , v) = do e' :: AUN.Exp TH.Name <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s) =>
         Cnv (TH.Exp , ET.Env TG.Typ s , ES.Env n TH.Name)
             (AUN.Exp TH.Name)
         where
  cnv (e , s , g) = do e' :: AUN.Exp TH.Name <- cnv (e , ())
                       etaPrms s g e'

instance Cnv (TH.Exp , es , en)
             TH.Exp
         where
  cnv (e , _ , _) = pure e

-----------------------------------------------------------------------
-- Conversion from AUN
-----------------------------------------------------------------------
instance (s ~ s' , n ~ Len s , TG.Type a') =>
         Cnv (AUN.Exp TH.Name , ET.Env TG.Typ s , ES.Env n TH.Name)
             (MFS.Exp s' a')
         where
  cnv (e , s , v) = do e' :: GHO.Exp s a' <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , t ~ t' , n ~ Len s , TG.Type a') =>
         Cnv (AUN.Exp TH.Name , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GHO.Exp s' a')
         where
  cnv (e , s , v) = do e' :: GFO.Exp s' '[] a' <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , TG.Type a') =>
         Cnv (AUN.Exp TH.Name , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GFO.Exp s' '[] a')
         where
  cnv (e , s , v) = do e' :: GTD.Exp n 'Zro TA.Typ <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n ~ Len s)  =>
         Cnv (AUN.Exp TH.Name , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GTD.Exp n 'Zro TA.Typ)
         where
  cnv (e , s , v) = do e' :: AUD.Exp <- cnv (e , s , v)
                       cnv (e' , s , v)

instance Cnv (AUN.Exp TH.Name , es , ES.Env n TH.Name)
             AUD.Exp
         where
  cnv (e , _ , vs) = do vs' :: EP.Env TH.Name <- cnv (vs , ())
                        cnv (e , (vs' , [] :: EP.Env TH.Name))

instance Cnv (AUN.Exp TH.Name , es , en)
             (AUN.Exp TH.Name)
         where
  cnv (e , _ , _ ) = pure e

-----------------------------------------------------------------------
-- Conversion from AUD
-----------------------------------------------------------------------
instance (s ~ s' , n ~ Len s , TG.Type a') =>
         Cnv (AUD.Exp , ET.Env TG.Typ s , ES.Env n TH.Name)
             (MFS.Exp s' a')
         where
  cnv (e , s , v) = do e' :: GHO.Exp s a' <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , TG.Type a') =>
         Cnv (AUD.Exp , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GHO.Exp s' a')
         where
  cnv (e , s , v) = do e' :: GFO.Exp s '[] a' <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , TG.Type a') =>
         Cnv (AUD.Exp , ET.Env TG.Typ s , ES.Env n TH.Name)
             (GFO.Exp s' '[] a')
         where
  cnv (e , s , v) = do e' :: GTD.Exp n 'Zro TA.Typ <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (n' ~ Len s) =>
         Cnv (AUD.Exp , ET.Env TG.Typ s , en)
             (GTD.Exp n' 'Zro TA.Typ)
         where
  cnv (e , s , _) = do e' :: GTD.Exp n' 'Zro (Maybe TA.Typ)
                             <- cnv (e , (ET.len s , NG.Zro))
                       s' :: ES.Env   n' TA.Typ <- cnv (s , ())
                       cnv (e' , (s', ES.Emp :: ES.Env 'Zro TA.Typ))

instance Cnv (AUD.Exp , es , en)
             AUD.Exp
         where
  cnv (e , _ , _ ) = pure e

-----------------------------------------------------------------------
-- Conversion from GTD
-----------------------------------------------------------------------
instance (s ~ s' , n ~ Len s , TG.Type a') =>
         Cnv (GTD.Exp n 'Zro TA.Typ , ET.Env TG.Typ s, ES.Env n TH.Name)
             (MFS.Exp s' a')
         where
  cnv (e , s , v) = do e' :: GHO.Exp s a' <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , TG.Type a') =>
         Cnv (GTD.Exp n 'Zro TA.Typ, ET.Env TG.Typ s , ES.Env n TH.Name)
             (GHO.Exp s' a')
         where
  cnv (e , s , v) = do e' :: GFO.Exp s '[] t <- cnv (e , s , v)
                       cnv (e' , s , v)

instance (s ~ s' , n ~ Len s , TG.Type a') =>
         Cnv (GTD.Exp n 'Zro TA.Typ , ET.Env TG.Typ s , en)
             (GFO.Exp s' '[] a')
         where
  cnv (e , s , _) = cnv (e , (s , ET.Emp :: ET.Env TG.Typ '[]))

instance (n ~ n') =>
         Cnv (GTD.Exp n  'Zro TA.Typ , es , en)
             (GTD.Exp n' 'Zro TA.Typ)
         where
  cnv (e , _ , _) = return e

-----------------------------------------------------------------------
-- Conversion from GFO
-----------------------------------------------------------------------
instance (s ~ s' , a ~ a' , n ~ Len s , TG.Type a') =>
         Cnv (GFO.Exp s '[] a , ET.Env TG.Typ s , en)
             (MFS.Exp s' a')
         where
  cnv (e , s , _) = do e' :: GHO.Exp s a <- cnv (e , s , ())
                       cnv (e' , s , ())

instance (s ~ s' , a ~ a' , n ~ Len s , TG.Type a') =>
         Cnv (GFO.Exp s '[] a , ET.Env TG.Typ s , en)
             (GHO.Exp s' a')
         where
  cnv (e , s , _) = cnv ({- nrm -} e , s)

instance (s ~ s' , a ~ a' , n ~ Len s , TG.Type a') =>
         Cnv (GFO.Exp s  '[] a , es , en)
             (GFO.Exp s' '[] a')
         where
  cnv (e , _ , _) = pure e

-----------------------------------------------------------------------
-- Conversion from GHO
-----------------------------------------------------------------------
instance (s ~ s' , a ~ a' , n ~ Len s , TG.Type a') =>
         Cnv (GHO.Exp s  a , ET.Env TG.Typ s , en)
             (MFS.Exp s' a')
         where
  cnv (e , s , _) = case getPrfHasSin s of
    PrfHasSin -> cnv ({-nrm (eta e)-} onHOAS nrm e , ())

instance (s ~ s' , a ~ a' , TG.Type a') =>
         Cnv (GHO.Exp s  a , es , en)
             (GHO.Exp s' a')
         where
  cnv (e , _ , _) = pure e

instance (s ~ s' , a ~ a' , n ~ Len s , TG.Type a') =>
         Cnv (GHO.Exp s  a , ET.Env TG.Typ s , en)
             (GFO.Exp s' '[] a')
         where
  cnv (e , s , _) = cnv (e , s)

-----------------------------------------------------------------------
-- Conversion from MFS
-----------------------------------------------------------------------
instance (s ~ s' , a ~ a' , n ~ Len s , TG.Type a') =>
         Cnv (MFS.Exp s  a , es , en)
             (MFS.Exp s' a')
         where
  cnv (e , _ , _) = pure e

instance (s ~ s' , a ~ a' , n ~ Len s , TG.Type a') =>
         Cnv (MFS.Exp s  a , es , en)
             (GHO.Exp s' a')
         where
  cnv (e , _ , _) = cnv (e , ())

instance (s ~ s' , a ~ a' , n ~ Len s , TG.Type a') =>
         Cnv (MFS.Exp s  a , ET.Env TG.Typ s , en)
             (GFO.Exp s' '[] a')
         where
  cnv (e , s , _) = do e' :: GHO.Exp s a <- cnv (e , ())
                       cnv (e' , s)
