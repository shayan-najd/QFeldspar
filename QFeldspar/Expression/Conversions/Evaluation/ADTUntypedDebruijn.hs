module QFeldspar.Expression.Conversions.Evaluation.ADTUntypedDebruijn
       () where

import QFeldspar.MyPrelude

import QFeldspar.Expression.ADTUntypedDebruijn
import qualified QFeldspar.Expression.ADTValue as FAV
import QFeldspar.Environment.Plain
import QFeldspar.Conversion

instance Cnv (Exp , (Env FAV.Exp , Env FAV.Exp)) FAV.Exp where
  cnv (ee , r@(s , g)) = join (case ee of
    Var x        -> FAV.var  <$> get x g
    Prm x es     -> FAV.prm  <$> get x s <*> mapM (\ e -> cnv (e , r)) es
    _ -> $(biGenOverloadedML 'ee ''Exp "FAV"
     ['Prm,'Var]
     (\ tt -> if
       | matchQ tt [t| Exp |] -> [| \ e -> cnv (e , r) |]
       | matchQ tt [t| Fun |] -> [| \ (Fun e) -> pure
                                        (\ v -> frmRgtZro (cnv (e  , (s , v : g)))) |]
       | otherwise            -> [| pure   |])))
