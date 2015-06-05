module QFeldspar.Expression.Conversions.TypeWithnessing () where

import QFeldspar.MyPrelude
import qualified QFeldspar.Expression.GADTTyped as FGTD
import qualified QFeldspar.Expression.GADTFirstOrder as FGFO
import qualified QFeldspar.Type.GADT as TFG
import qualified QFeldspar.Type.ADT as TFA
import QFeldspar.Environment.Typed
import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()
import QFeldspar.Type.Conversion ()
import QFeldspar.Singleton
import qualified QFeldspar.Variable.Typed as VT

type ExsTyp = ExsSin TFG.Typ

cnvEnv :: forall s g d. TFG.Types d =>
          (Env TFG.Typ s , Env TFG.Typ g) ->
          [FGTD.Exp (Len s) (Len g) TFA.Typ] -> NamM ErrM (Env (FGFO.Exp s g) d)
cnvEnv _ []       = case sin :: Env TFG.Typ d of
   Emp           -> return Emp
   Ext _ _       -> fail "Type Error!"
cnvEnv r (e : es) = case sin :: Env TFG.Typ d of
   Emp           -> fail "Type Error!"
   Ext a as      -> case (getPrfHasSin a , getPrfHasSin as) of
    (PrfHasSin , PrfHasSin) -> Ext <$> cnv (e , r) <*> cnvEnv r es

instance (s ~ s' , g ~ g' , m ~ (Len s) , n ~ (Len g) , TFG.Type a) =>
         Cnv (FGTD.Exp m n TFA.Typ , (Env TFG.Typ s , Env TFG.Typ g))
             (FGFO.Exp s' g' a) where
  cnv (ee , r@(s , g)) = let t = sin :: TFG.Typ a in case ee of
    FGTD.ConI i       -> case t of
      TFG.Wrd         -> pure (FGFO.ConI i)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.ConB b       -> case t of
      TFG.Bol         -> pure (FGFO.ConB b)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.ConF f       -> case t of
      TFG.Flt         -> pure (FGFO.ConF f)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Prm tx x ns  -> do ExsSin (as :: Env TFG.Typ as):: ExsSin (Env TFG.Typ) <- cnv (tx , ())
                            PrfHasSin <- getPrfHasSinM as
                            ns' :: Env (FGFO.Exp s g) as <- cnvEnv r ns
                            PrfHasSin <- getPrfHasSinM (TFG.cur as t)
                            x' :: VT.Var s (as TFG.:-> a) <- cnv (x , s)
                            return (FGFO.Prm x' ns')
    FGTD.Abs eb       -> case t of
      TFG.Arr ta _    -> case TFG.getPrfHasSinArr t of
       (PrfHasSin , PrfHasSin) -> FGFO.Abs <$> cnvWth r (ta , eb)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Tpl ef es    -> case t of
      TFG.Tpl _ _     -> case TFG.getPrfHasSinTpl t of
       (PrfHasSin , PrfHasSin) -> FGFO.Tpl <$> cnvWth r ef <*> cnvWth r es
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Ary el ef    -> case t of
      TFG.Ary _       -> case TFG.getPrfHasSinAry t of
       PrfHasSin      -> FGFO.Ary <$> cnvWth r el <*> cnvWth r ef
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Len ta e     -> case t of
      TFG.Wrd         -> do ExsSin ta' :: ExsTyp <- cnv ta
                            PrfHasSin <- getPrfHasSinM ta'
                            e' <- cnvWth r e
                            FGFO.Len <$> pure
                              (samTyp (TFG.Ary ta') e')
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.AryV el ef   -> case t of
      TFG.Vct _       -> case TFG.getPrfHasSinVec t of
       PrfHasSin      -> FGFO.AryV <$> cnvWth r el <*> cnvWth r ef
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.LenV ta e    -> case t of
      TFG.Wrd         -> do ExsSin ta' :: ExsTyp <- cnv ta
                            PrfHasSin <- getPrfHasSinM ta'
                            e' <- cnvWth r e
                            FGFO.LenV <$> pure
                              (samTyp (TFG.Vct ta') e')
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Cmx er ei    -> case t of
      TFG.Cmx         -> FGFO.Cmx <$> cnvWth r er <*> cnvWth r ei
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)

    FGTD.Mul er ei    -> case t of
      TFG.Wrd         -> FGFO.Mul <$> cnvWth r er <*> cnvWth r ei
      TFG.Flt         -> FGFO.Mul <$> cnvWth r er <*> cnvWth r ei
      TFG.Cmx         -> FGFO.Mul <$> cnvWth r er <*> cnvWth r ei
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)

    FGTD.Add er ei    -> case t of
      TFG.Wrd         -> FGFO.Add <$> cnvWth r er <*> cnvWth r ei
      TFG.Flt         -> FGFO.Add <$> cnvWth r er <*> cnvWth r ei
      TFG.Cmx         -> FGFO.Add <$> cnvWth r er <*> cnvWth r ei
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Sub er ei    -> case t of
      TFG.Wrd         -> FGFO.Sub <$> cnvWth r er <*> cnvWth r ei
      TFG.Flt         -> FGFO.Sub <$> cnvWth r er <*> cnvWth r ei
      TFG.Cmx         -> FGFO.Sub <$> cnvWth r er <*> cnvWth r ei
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Non          -> case t of
     TFG.May _        -> case TFG.getPrfHasSinMay t of
      PrfHasSin       -> pure FGFO.Non
     _                -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Som e        -> case t of
     TFG.May _        -> case TFG.getPrfHasSinMay t of
      PrfHasSin       -> FGFO.Som <$> cnvWth r e
     _                -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Var x        -> FGFO.Var  <$> cnv (x , g)
    FGTD.App ta ef ea -> do ExsSin ta' :: ExsTyp <- cnv ta
                            PrfHasSin <- getPrfHasSinM ta'
                            ea' <- cnvWth r ea
                            FGFO.App <$> cnvWth r ef
                                     <*> pure (samTyp ta' ea')
    FGTD.Cnd ec et ef -> FGFO.Cnd <$> cnvWth r ec <*> cnvWth r et <*> cnvWth r ef
    FGTD.Whl ec eb ei -> FGFO.Whl <$> cnvWth r ec <*> cnvWth r eb <*> cnvWth r ei
    FGTD.Fst ts e     -> do ExsSin ts' <- cnv ts
                            PrfHasSin  <- getPrfHasSinM ts'
                            e'         <- cnvWth r e
                            FGFO.Fst <$> pure
                                    (samTyp (TFG.Tpl t ts') e')
    FGTD.Snd tf e     -> do ExsSin tf' <- cnv tf
                            PrfHasSin  <- getPrfHasSinM tf'
                            e'         <- cnvWth r e
                            FGFO.Snd <$> pure
                                    (samTyp (TFG.Tpl tf' t) e')
    FGTD.Ind e  ei    -> FGFO.Ind  <$> cnvWth r e <*> cnvWth r ei
    FGTD.IndV e  ei   -> FGFO.IndV <$> cnvWth r e <*> cnvWth r ei
    FGTD.LeT tl el eb -> do ExsSin tl' :: ExsTyp <- cnv tl
                            PrfHasSin <- getPrfHasSinM tl'
                            FGFO.LeT <$> cnvWth r el <*> cnvWth r (tl' , eb)
    FGTD.Eql tl el eb -> case t of
      TFG.Bol         -> do ExsSin tl' :: ExsTyp <- cnv tl
                            PrfHasSin <- getPrfHasSinM tl'
                            case tl' of
                              TFG.Bol -> do el' <- cnvWth r el
                                            FGFO.Eql <$> pure (samTyp tl' el') <*> cnvWth r eb
                              TFG.Wrd -> do el' <- cnvWth r el
                                            FGFO.Eql <$> pure (samTyp tl' el') <*> cnvWth r eb
                              TFG.Flt -> do el' <- cnvWth r el
                                            FGFO.Eql <$> pure (samTyp tl' el') <*> cnvWth r eb
                              _       -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Ltd tl el eb -> case t of
      TFG.Bol         -> do ExsSin tl' :: ExsTyp <- cnv tl
                            PrfHasSin <- getPrfHasSinM tl'
                            case tl' of
                              TFG.Bol -> do el' <- cnvWth r el
                                            FGFO.Ltd <$> pure (samTyp tl' el') <*> cnvWth r eb
                              TFG.Wrd -> do el' <- cnvWth r el
                                            FGFO.Ltd <$> pure (samTyp tl' el') <*> cnvWth r eb
                              TFG.Flt -> do el' <- cnvWth r el
                                            FGFO.Ltd <$> pure (samTyp tl' el') <*> cnvWth r eb
                              _       -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.May tm em en es -> do ExsSin t' :: ExsTyp <- cnv tm
                               PrfHasSin <- getPrfHasSinM t'
                               em' <- cnvWth r em
                               FGFO.May
                                       <$> pure (samTyp (TFG.May t') em')
                                       <*> cnvWth r en <*> cnvWth r es
    FGTD.Typ _ e      -> cnvWth r e
    FGTD.Int i        -> case t of
      TFG.Wrd         -> pure (FGFO.Int i)
      TFG.Flt         -> pure (FGFO.Int i)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Mem e        -> FGFO.Mem <$> cnvWth r e
    FGTD.Fix e        -> FGFO.Fix <$> cnvWth r e

instance (s ~ s' , g ~ g' , a ~ a' , n ~ (Len (a ': g)) , m ~ (Len s), HasSin TFG.Typ b) =>
         Cnv ((TFG.Typ a , FGTD.Exp m n TFA.Typ) , (Env TFG.Typ s , Env TFG.Typ g ))
             (FGFO.Exp s' (a' ': g') b) where
  cnv ((t , ee) , (s , g)) = cnv (ee , (s ,  Ext t g))
