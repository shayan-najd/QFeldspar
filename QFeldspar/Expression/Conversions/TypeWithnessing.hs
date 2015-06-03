module QFeldspar.Expression.Conversions.TypeWithnessing () where

import QFeldspar.MyPrelude
import qualified QFeldspar.Expression.GADTTyped as FGTD
import qualified QFeldspar.Expression.GADTFirstOrder as FGFO
import qualified QFeldspar.Type.GADT as TFG
import qualified QFeldspar.Type.ADT as TFA
import QFeldspar.Environment.Typed
import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()
import QFeldspar.Singleton
import qualified QFeldspar.Variable.Typed as VT

type ExsTyp = ExsSin TFG.Typ

cnvEnv :: forall a s g. TFG.Type a => (Env TFG.Typ s , Env TFG.Typ g) ->
     TFG.Typ a -> [FGTD.Exp (Len s) (Len g) TFA.Typ] -> NamM ErrM (Env (FGFO.Exp s g) (TFG.Arg a) , TFG.Typ (TFG.Out a))
cnvEnv r t ess = case t of
  TFG.Arr _ b -> case ess of
    e : es    -> case TFG.getPrfHasSinArr t of
       (PrfHasSin , PrfHasSin) -> do e' <- cnv (e , r)
                                     (es' , to) <- cnvEnv r b es
                                     return (Ext e' es' , to)
    _         -> fail "Type Error!"
  _           -> case obvious :: TFG.Arg a :~: '[] of
    Rfl       -> case obvious :: TFG.Out a :~: a   of
      Rfl     -> case ess of
        []    -> return (Emp , t)
        _     -> fail "Type Error!"

instance (s ~ s' , g ~ g' , m ~ (Len s) , n ~ (Len g) , HasSin TFG.Typ a) =>
         Cnv (FGTD.Exp m n TFA.Typ , (Env TFG.Typ s , Env TFG.Typ g))
             (FGFO.Exp s' g' a) where
  cnv (ee , r@(s , g)) = let ?r = r in let t = sin :: TFG.Typ a in case ee of
    FGTD.ConI i       -> case t of
      TFG.Wrd         -> pure (FGFO.ConI i)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.ConB b       -> case t of
      TFG.Bol         -> pure (FGFO.ConB b)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.ConF f       -> case t of
      TFG.Flt         -> pure (FGFO.ConF f)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Prm tx x ns  -> do ExsSin (tx' :: TFG.Typ tx) :: ExsTyp <- cnv tx
                            PrfHasSin <- getPrfHasSinM tx'
                            x' :: VT.Var s tx <- cnv (x , s)
                            case tx' of
                              TFG.Arr _ _ -> do (ns' , to) <- cnvEnv r tx' ns
                                                case eqlSin to t of
                                                  Rgt Rfl -> return (FGFO.Prm x' ns')
                                                  _       -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
                              _           -> case eqlSin tx' t of
                                Rgt Rfl   -> case obvious :: TFG.Arg tx :~: '[] of
                                  Rfl     -> case obvious :: TFG.Out tx :~: tx  of
                                    Rfl   -> pure (FGFO.Prm x' Emp)
                                _         -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Abs eb       -> case t of
      TFG.Arr ta _    -> case TFG.getPrfHasSinArr t of
       (PrfHasSin , PrfHasSin) -> FGFO.Abs <$@> (ta , eb)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Tpl ef es    -> case t of
      TFG.Tpl _ _     -> case TFG.getPrfHasSinTpl t of
       (PrfHasSin , PrfHasSin) -> FGFO.Tpl <$@> ef <*@> es
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Ary el ef    -> case t of
      TFG.Ary _       -> case TFG.getPrfHasSinAry t of
       PrfHasSin      -> FGFO.Ary <$@> el <*@> ef
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Len ta e     -> case t of
      TFG.Wrd         -> do ExsSin ta' :: ExsTyp <- cnv ta
                            PrfHasSin <- getPrfHasSinM ta'
                            e' <- cnvImp e
                            FGFO.Len <$> pure
                              (samTyp (TFG.Ary ta') e')
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.AryV el ef   -> case t of
      TFG.Vct _       -> case TFG.getPrfHasSinVec t of
       PrfHasSin      -> FGFO.AryV <$@> el <*@> ef
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.LenV ta e    -> case t of
      TFG.Wrd         -> do ExsSin ta' :: ExsTyp <- cnv ta
                            PrfHasSin <- getPrfHasSinM ta'
                            e' <- cnvImp e
                            FGFO.LenV <$> pure
                              (samTyp (TFG.Vct ta') e')
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Cmx er ei    -> case t of
      TFG.Cmx         -> FGFO.Cmx <$@> er <*@> ei
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)

    FGTD.Mul er ei    -> case t of
      TFG.Wrd         -> FGFO.Mul <$@> er <*@> ei
      TFG.Flt         -> FGFO.Mul <$@> er <*@> ei
      TFG.Cmx         -> FGFO.Mul <$@> er <*@> ei
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)

    FGTD.Add er ei    -> case t of
      TFG.Wrd         -> FGFO.Add <$@> er <*@> ei
      TFG.Flt         -> FGFO.Add <$@> er <*@> ei
      TFG.Cmx         -> FGFO.Add <$@> er <*@> ei
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Sub er ei    -> case t of
      TFG.Wrd         -> FGFO.Sub <$@> er <*@> ei
      TFG.Flt         -> FGFO.Sub <$@> er <*@> ei
      TFG.Cmx         -> FGFO.Sub <$@> er <*@> ei
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Non          -> case t of
     TFG.May _        -> case TFG.getPrfHasSinMay t of
      PrfHasSin       -> pure FGFO.Non
     _                -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Som e        -> case t of
     TFG.May _        -> case TFG.getPrfHasSinMay t of
      PrfHasSin       -> FGFO.Som <$@> e
     _                -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Var x        -> FGFO.Var  <$> cnv (x , g)
    FGTD.App ta ef ea -> do ExsSin ta' :: ExsTyp <- cnv ta
                            PrfHasSin <- getPrfHasSinM ta'
                            ea' <- cnvImp ea
                            FGFO.App <$@> ef
                                    <*> pure (samTyp ta' ea')
    FGTD.Cnd ec et ef -> FGFO.Cnd <$@> ec <*@> et <*@> ef
    FGTD.Whl ec eb ei -> FGFO.Whl <$@> ec <*@> eb <*@> ei
    FGTD.Fst ts e     -> do ExsSin ts' <- cnv ts
                            PrfHasSin  <- getPrfHasSinM ts'
                            e'         <- cnvImp e
                            FGFO.Fst <$> pure
                                    (samTyp (TFG.Tpl t ts') e')
    FGTD.Snd tf e     -> do ExsSin tf' <- cnv tf
                            PrfHasSin  <- getPrfHasSinM tf'
                            e'         <- cnvImp e
                            FGFO.Snd <$> pure
                                    (samTyp (TFG.Tpl tf' t) e')
    FGTD.Ind e  ei    -> FGFO.Ind <$@> e  <*@> ei
    FGTD.IndV e  ei   -> FGFO.IndV <$@> e  <*@> ei
    FGTD.Let tl el eb -> do ExsSin tl' :: ExsTyp <- cnv tl
                            PrfHasSin <- getPrfHasSinM tl'
                            FGFO.Let <$@> el <*@> (tl' , eb)
    FGTD.Eql tl el eb -> case t of
      TFG.Bol         -> do ExsSin tl' :: ExsTyp <- cnv tl
                            PrfHasSin <- getPrfHasSinM tl'
                            case tl' of
                              TFG.Bol -> do el' <- cnvImp el
                                            FGFO.Eql <$> pure (samTyp tl' el') <*@> eb
                              TFG.Wrd -> do el' <- cnvImp el
                                            FGFO.Eql <$> pure (samTyp tl' el') <*@> eb
                              TFG.Flt -> do el' <- cnvImp el
                                            FGFO.Eql <$> pure (samTyp tl' el') <*@> eb
                              _       -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Ltd tl el eb -> case t of
      TFG.Bol         -> do ExsSin tl' :: ExsTyp <- cnv tl
                            PrfHasSin <- getPrfHasSinM tl'
                            case tl' of
                              TFG.Bol -> do el' <- cnvImp el
                                            FGFO.Ltd <$> pure (samTyp tl' el') <*@> eb
                              TFG.Wrd -> do el' <- cnvImp el
                                            FGFO.Ltd <$> pure (samTyp tl' el') <*@> eb
                              TFG.Flt -> do el' <- cnvImp el
                                            FGFO.Ltd <$> pure (samTyp tl' el') <*@> eb
                              _       -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.May tm em en es -> do ExsSin t' :: ExsTyp <- cnv tm
                               PrfHasSin <- getPrfHasSinM t'
                               em' <- cnvImp em
                               FGFO.May
                                       <$> pure (samTyp (TFG.May t') em')
                                       <*@> en <*@> es
    FGTD.Typ _ e      -> cnvImp e
    FGTD.Int i        -> case t of
      TFG.Wrd         -> pure (FGFO.Int i)
      TFG.Flt         -> pure (FGFO.Int i)
      _               -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)
    FGTD.Mem e        -> FGFO.Mem <$@> e
    FGTD.Fix e        -> FGFO.Fix <$@> e

instance (s ~ s' , g ~ g' , a ~ a' , n ~ (Len (a ': g)) , m ~ (Len s), HasSin TFG.Typ b) =>
         Cnv ((TFG.Typ a , FGTD.Exp m n TFA.Typ) , (Env TFG.Typ s , Env TFG.Typ g ))
             (FGFO.Exp s' (a' ': g') b) where
  cnv ((t , ee) , (s , g)) = cnv (ee , (s ,  Ext t g))
