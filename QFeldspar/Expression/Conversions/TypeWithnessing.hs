module QFeldspar.Expression.Conversions.TypeWithnessing () where

import QFeldspar.MyPrelude
import qualified QFeldspar.Expression.GADTTyped as GTD
import qualified QFeldspar.Expression.GADTFirstOrder as GFO
import qualified QFeldspar.Type.GADT as TG
import qualified QFeldspar.Type.ADT as TA
import QFeldspar.Environment.Typed
import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()
import QFeldspar.Type.Conversion ()
import QFeldspar.Singleton
import qualified QFeldspar.Variable.Typed as VT

type ExsTyp = ExsSin TG.Typ

cnvEnv :: forall s g d. TG.Types d =>
          (Env TG.Typ s , Env TG.Typ g) ->
          [GTD.Exp (Len s) (Len g) TA.Typ] -> NamM ErrM (Env (GFO.Exp s g) d)
cnvEnv r ess = let d = sin :: Env TG.Typ d in case ess of
  []        -> case d of
   Emp      -> return Emp
   Ext _ _  -> fail "Type Error!"
  e : es    -> case d of
   Emp      -> fail "Type Error!"
   Ext _ _  -> case TG.getPrfHasSinEnvOf d of
    (PrfHasSin , PrfHasSin) -> Ext <$> cnv (e , r) <*> cnvEnv r es

instance (s ~ s' , g ~ g' , m ~ (Len s) , n ~ (Len g) , TG.Type a) =>
         Cnv (GTD.Exp m n TA.Typ , (Env TG.Typ s , Env TG.Typ g))
             (GFO.Exp s' g' a) where
  cnv (ee , r@(s , g)) = let t = sin :: TG.Typ a in case ee of
    GTD.ConI i       -> case t of
      TG.Wrd         -> pure (GFO.ConI i)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.ConB b       -> case t of
      TG.Bol         -> pure (GFO.ConB b)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.ConF f       -> case t of
      TG.Flt         -> pure (GFO.ConF f)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.Prm tx x ns  -> do ExsSin (as :: Env TG.Typ as):: ExsSin (Env TG.Typ) <- cnv (tx , ())
                           PrfHasSin <- getPrfHasSinM as
                           ns' :: Env (GFO.Exp s g) as <- cnvEnv r ns
                           PrfHasSin <- getPrfHasSinM (TG.cur as t)
                           x' :: VT.Var s (as TG.:-> a) <- cnv (x , s)
                           return (GFO.Prm x' ns')
    GTD.Abs eb       -> case t of
      TG.Arr ta _    -> case TG.getPrfHasSinArr t of
       (PrfHasSin , PrfHasSin) -> GFO.Abs <$> cnvWth r (ta , eb)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.Tpl ef es    -> case t of
      TG.Tpl _ _     -> case TG.getPrfHasSinTpl t of
       (PrfHasSin , PrfHasSin) -> GFO.Tpl <$> cnvWth r ef <*> cnvWth r es
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.Ary el ef    -> case t of
      TG.Ary _       -> case TG.getPrfHasSinAry t of
       PrfHasSin      -> GFO.Ary <$> cnvWth r el <*> cnvWth r ef
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.Len ta e     -> case t of
      TG.Wrd         -> do ExsSin ta' :: ExsTyp <- cnv ta
                           PrfHasSin <- getPrfHasSinM ta'
                           e' <- cnvWth r e
                           GFO.Len <$> pure
                               (samTyp (TG.Ary ta') e')
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.AryV el ef   -> case t of
      TG.Vct _       -> case TG.getPrfHasSinVec t of
       PrfHasSin      -> GFO.AryV <$> cnvWth r el <*> cnvWth r ef
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.LenV ta e    -> case t of
      TG.Wrd         -> do ExsSin ta' :: ExsTyp <- cnv ta
                           PrfHasSin <- getPrfHasSinM ta'
                           e' <- cnvWth r e
                           GFO.LenV <$> pure
                               (samTyp (TG.Vct ta') e')
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.Cmx er ei    -> case t of
      TG.Cmx         -> GFO.Cmx <$> cnvWth r er <*> cnvWth r ei
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)

    GTD.Mul er ei    -> case t of
      TG.Wrd         -> GFO.Mul <$> cnvWth r er <*> cnvWth r ei
      TG.Flt         -> GFO.Mul <$> cnvWth r er <*> cnvWth r ei
      TG.Cmx         -> GFO.Mul <$> cnvWth r er <*> cnvWth r ei
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)

    GTD.Add er ei    -> case t of
      TG.Wrd         -> GFO.Add <$> cnvWth r er <*> cnvWth r ei
      TG.Flt         -> GFO.Add <$> cnvWth r er <*> cnvWth r ei
      TG.Cmx         -> GFO.Add <$> cnvWth r er <*> cnvWth r ei
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.Sub er ei    -> case t of
      TG.Wrd         -> GFO.Sub <$> cnvWth r er <*> cnvWth r ei
      TG.Flt         -> GFO.Sub <$> cnvWth r er <*> cnvWth r ei
      TG.Cmx         -> GFO.Sub <$> cnvWth r er <*> cnvWth r ei
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.Non          -> case t of
     TG.May _        -> case TG.getPrfHasSinMay t of
      PrfHasSin       -> pure GFO.Non
     _                -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.Som e        -> case t of
     TG.May _        -> case TG.getPrfHasSinMay t of
      PrfHasSin       -> GFO.Som <$> cnvWth r e
     _                -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.Var x        -> GFO.Var  <$> cnv (x , g)
    GTD.App ta ef ea -> do ExsSin ta' :: ExsTyp <- cnv ta
                           PrfHasSin <- getPrfHasSinM ta'
                           ea' <- cnvWth r ea
                           GFO.App <$> cnvWth r ef
                                  <*> pure (samTyp ta' ea')
    GTD.Cnd ec et ef -> GFO.Cnd <$> cnvWth r ec <*> cnvWth r et <*> cnvWth r ef
    GTD.Whl ec eb ei -> GFO.Whl <$> cnvWth r ec <*> cnvWth r eb <*> cnvWth r ei
    GTD.Fst ts e     -> do ExsSin ts' <- cnv ts
                           PrfHasSin  <- getPrfHasSinM ts'
                           e'         <- cnvWth r e
                           GFO.Fst <$> pure
                                    (samTyp (TG.Tpl t ts') e')
    GTD.Snd tf e     -> do ExsSin tf' <- cnv tf
                           PrfHasSin  <- getPrfHasSinM tf'
                           e'         <- cnvWth r e
                           GFO.Snd <$> pure
                                    (samTyp (TG.Tpl tf' t) e')
    GTD.Ind e  ei    -> GFO.Ind  <$> cnvWth r e <*> cnvWth r ei
    GTD.IndV e  ei   -> GFO.IndV <$> cnvWth r e <*> cnvWth r ei
    GTD.LeT tl el eb -> do ExsSin tl' :: ExsTyp <- cnv tl
                           PrfHasSin <- getPrfHasSinM tl'
                           GFO.LeT <$> cnvWth r el <*> cnvWth r (tl' , eb)
    GTD.Eql tl el eb -> case t of
      TG.Bol         -> do ExsSin tl' :: ExsTyp <- cnv tl
                           PrfHasSin <- getPrfHasSinM tl'
                           case tl' of
                              TG.Bol -> do el' <- cnvWth r el
                                           GFO.Eql <$> pure (samTyp tl' el') <*> cnvWth r eb
                              TG.Wrd -> do el' <- cnvWth r el
                                           GFO.Eql <$> pure (samTyp tl' el') <*> cnvWth r eb
                              TG.Flt -> do el' <- cnvWth r el
                                           GFO.Eql <$> pure (samTyp tl' el') <*> cnvWth r eb
                              _       -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.Ltd tl el eb -> case t of
      TG.Bol         -> do ExsSin tl' :: ExsTyp <- cnv tl
                           PrfHasSin <- getPrfHasSinM tl'
                           case tl' of
                              TG.Bol -> do el' <- cnvWth r el
                                           GFO.Ltd <$> pure (samTyp tl' el') <*> cnvWth r eb
                              TG.Wrd -> do el' <- cnvWth r el
                                           GFO.Ltd <$> pure (samTyp tl' el') <*> cnvWth r eb
                              TG.Flt -> do el' <- cnvWth r el
                                           GFO.Ltd <$> pure (samTyp tl' el') <*> cnvWth r eb
                              _       -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.May tm em en es -> do ExsSin t' :: ExsTyp <- cnv tm
                              PrfHasSin <- getPrfHasSinM t'
                              em' <- cnvWth r em
                              GFO.May <$> pure (samTyp (TG.May t') em')
                                      <*> cnvWth r en <*> cnvWth r es
    GTD.Typ _ e      -> cnvWth r e
    GTD.Int i        -> case t of
      TG.Wrd         -> pure (GFO.Int i)
      TG.Flt         -> pure (GFO.Int i)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    GTD.Mem e        -> GFO.Mem <$> cnvWth r e
    GTD.Fix e        -> GFO.Fix <$> cnvWth r e

instance (s ~ s' , g ~ g' , a ~ a' , n ~ (Len (a ': g)) , m ~ (Len s), TG.Type b) =>
         Cnv ((TG.Typ a , GTD.Exp m n TA.Typ) , (Env TG.Typ s , Env TG.Typ g ))
             (GFO.Exp s' (a' ': g') b) where
  cnv ((t , ee) , (s , g)) = cnv (ee , (s ,  Ext t g))
