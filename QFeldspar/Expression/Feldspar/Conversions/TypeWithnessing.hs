module QFeldspar.Expression.Feldspar.Conversions.TypeWithnessing () where

import QFeldspar.MyPrelude

import qualified QFeldspar.Expression.Feldspar.GADTTyped      as FGTD
import qualified QFeldspar.Expression.Feldspar.GADTFirstOrder as FGFO

import qualified QFeldspar.Type.Feldspar.GADT                 as TFG
import qualified QFeldspar.Type.Feldspar.ADT                  as TFA

import QFeldspar.Environment.Typed

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion      ()

import QFeldspar.Singleton

type ExsTyp = ExsSin TFG.Typ

instance (r ~ r' , n ~ Len r , HasSin TFG.Typ t) =>
         Cnv (FGTD.Exp n TFA.Typ , Env TFG.Typ r)
             (FGFO.Exp r' t) where
  cnv (ee , r) = let ?r = r in let t = sin :: TFG.Typ t in case (ee , t) of
    (FGTD.ConI i       , TFG.Int)      -> pure (FGFO.ConI i)
    (FGTD.ConB b       , TFG.Bol)      -> pure (FGFO.ConB b)
    (FGTD.ConF f       , TFG.Flt)      -> pure (FGFO.ConF f)
    (FGTD.Var x        , _)            -> FGFO.Var  <$@> x
    (FGTD.Abs eb       , TFG.Arr ta _) -> case TFG.getPrfHasSinArr t of
      (PrfHasSin , PrfHasSin)          -> FGFO.Abs  <$@> (ta , eb)
    (FGTD.App ta ef ea , _)            -> do ExsSin ta' :: ExsTyp <- cnv ta
                                             PrfHasSin <- getPrfHasSinM ta'
                                             ea' <- cnvImp ea
                                             FGFO.App <$@> ef
                                                       <*> pure (samTyp ta' ea')
    (FGTD.Cnd ec et ef , _)            -> FGFO.Cnd <$@> ec <*@> et <*@> ef
    (FGTD.Whl ec eb ei , _)            -> FGFO.Whl <$@> ec <*@> eb <*@> ei
    (FGTD.Tpl ef es    , TFG.Tpl _ _)  -> case TFG.getPrfHasSinTpl t of
      (PrfHasSin , PrfHasSin)          -> FGFO.Tpl <$@> ef <*@> es
    (FGTD.Fst ts e     , _)            -> do ExsSin ts' <- cnv ts
                                             PrfHasSin  <- getPrfHasSinM ts'
                                             e'         <- cnvImp e
                                             FGFO.Fst <$> pure
                                                      (samTyp (TFG.Tpl t ts') e')
    (FGTD.Snd tf e     , _)            -> do ExsSin tf' <- cnv tf
                                             PrfHasSin  <- getPrfHasSinM tf'
                                             e'         <- cnvImp e
                                             FGFO.Snd <$> pure
                                                      (samTyp (TFG.Tpl tf' t) e')
    (FGTD.Ary el ef    , TFG.Ary _)    -> case TFG.getPrfHasSinAry t of
      PrfHasSin                        -> FGFO.Ary <$@> el <*@> ef
    (FGTD.Len ta e     , TFG.Int )     -> do ExsSin ta' :: ExsTyp <- cnv ta
                                             PrfHasSin <- getPrfHasSinM ta'
                                             e' <- cnvImp e
                                             FGFO.Len <$> pure
                                                      (samTyp (TFG.Ary ta') e')
    (FGTD.Ind e  ei    , _)            -> FGFO.Ind <$@> e  <*@> ei
    (FGTD.AryV el ef    , TFG.Vct _)   -> case TFG.getPrfHasSinVec t of
      PrfHasSin                        -> FGFO.AryV <$@> el <*@> ef
    (FGTD.LenV ta e     , TFG.Int )    -> do ExsSin ta' :: ExsTyp <- cnv ta
                                             PrfHasSin <- getPrfHasSinM ta'
                                             e' <- cnvImp e
                                             FGFO.LenV <$> pure
                                                      (samTyp (TFG.Vct ta') e')
    (FGTD.IndV e  ei    , _)           -> FGFO.IndV <$@> e  <*@> ei
    (FGTD.Cmx er ei    , TFG.Cmx)      -> FGFO.Cmx <$@> er <*@> ei
    (FGTD.Mul er ei    , TFG.Int)      -> FGFO.Mul <$@> er <*@> ei
    (FGTD.Mul er ei    , TFG.Flt)      -> FGFO.Mul <$@> er <*@> ei
    (FGTD.Let tl el eb , _)            -> do ExsSin tl' :: ExsTyp <- cnv tl
                                             PrfHasSin <- getPrfHasSinM tl'
                                             FGFO.Let <$@> el <*@> (tl' , eb)
    (FGTD.Non          , TFG.May _)    -> case TFG.getPrfHasSinMay t of
      PrfHasSin                        -> pure FGFO.Non
    (FGTD.Som e        , TFG.May _)    -> case TFG.getPrfHasSinMay t of
      PrfHasSin                        -> FGFO.Som <$@> e
    (FGTD.May tm em en es,_        )   -> do ExsSin t' :: ExsTyp <- cnv tm
                                             PrfHasSin <- getPrfHasSinM t'
                                             em' <- cnvImp em
                                             FGFO.May
                                               <$> pure (samTyp (TFG.May t') em')
                                               <*@> en <*@> es
    (FGTD.Typ _ e      , _)            -> cnvImp e
    _                                  -> fail ("Type Error!\n" ++ show ee ++ " :: " ++ show t)

instance (r ~ r' , n ~ Len (tr ': r) , HasSin TFG.Typ t , tr ~ tr') =>
         Cnv ((TFG.Typ tr , FGTD.Exp n TFA.Typ) , Env TFG.Typ r)
             (FGFO.Exp (tr' ': r') t) where
  cnv ((t , ee) , r) = cnv (ee , Ext t r)
