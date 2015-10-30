module QFeldspar.Expression.Conversions.Evaluation.GADTFirstOrder () where

import QFeldspar.MyPrelude
import QFeldspar.Expression.GADTFirstOrder
import qualified QFeldspar.Expression.GADTValue as FGV
import qualified QFeldspar.Type.GADT as TG
import QFeldspar.Environment.Typed
import QFeldspar.Conversion
import QFeldspar.Variable.Conversion ()

instance (TG.Type a', a ~ a') =>
         Cnv (Exp s g a , (Env FGV.Exp s, Env FGV.Exp g)) (FGV.Exp a')
  where
  cnv (ee , r@(s , g)) = case ee of
    Var x    -> pure (get x g)
    Prm x es -> FGV.prm (get x s) <$> TG.mapMC (cnvWth r) es
    _        -> $(biGenOverloadedML 'ee ''Exp "FGV" ['Var,'Prm]
     (\ tt -> if
       | matchQ tt [t| Exp a (a ': a) a |] ->
           [| \ e -> (pure . FGV.Exp)
                (\ v -> FGV.getTrm
                  (frmRgtZro (cnv (e , (s , Ext (FGV.Exp v) g)))))|]
       | matchQ tt [t| Exp a a a |] -> [| cnvWth r |]
       | otherwise                  -> [| pure |]))
