{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Expression.Conversions.Lifting () where

import QFeldspar.MyPrelude
import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()
import qualified QFeldspar.Expression.GADTFirstOrder        as FGFO
import qualified QFeldspar.Expression.GADTHigherOrder       as FGHO
import qualified QFeldspar.Type.GADT                        as TFG
import QFeldspar.Variable.Typed                             as VT
import QFeldspar.Environment.Typed                          as ET
import qualified QFeldspar.Expression.Utils.GADTHigherOrder as FGHO
                 (sucAll,prdAll)

instance (t ~ t' , r ~ r') =>
         Cnv (FGFO.Exp r t , Env TFG.Typ r) (FGHO.Exp r' t') where
  cnv (e , r) = pure (cnv' ((ET.fmap FGHO.Var . cnvGEnvtoGVar) r) e)

cnvGEnvtoGVar :: Env tf r -> Env (VT.Var r) r
cnvGEnvtoGVar ET.Emp        = ET.Emp
cnvGEnvtoGVar (ET.Ext _ xs) = ET.Ext VT.Zro
                              (ET.fmap VT.Suc (cnvGEnvtoGVar xs))

cnv' :: forall r t.
        Env (FGHO.Exp r) r -> FGFO.Exp r t -> FGHO.Exp r t
cnv' r ee  = case ee of
  FGFO.Var v -> ET.get v r
  _          -> $(biGenOverloaded 'ee ''FGFO.Exp "FGHO" ['FGFO.Var]
   (\ tt -> if
    | matchQ tt [t| FGFO.Exp (t ': t) t |] -> [| cnv'F r |]
    | matchQ tt [t| FGFO.Exp t t |]        -> [| cnv'  r |]
    | otherwise                            -> [| id |]))

cnv'F :: Env (FGHO.Exp r) r -> FGFO.Exp (ta ': r) tb ->
         (FGHO.Exp r ta -> FGHO.Exp r tb)
cnv'F r f = (\ x ->
            FGHO.prdAll
            (cnv' (ET.fmap FGHO.sucAll
                      (Ext x r)) f))
