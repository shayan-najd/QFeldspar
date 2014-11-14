module QFeldspar.Expression.Utils.Common where
import QFeldspar.MyPrelude
import QFeldspar.Type.GADT
import QFeldspar.Singleton
import qualified Language.Haskell.TH as TH

trvWrp :: TH.Name -> TH.Name -> TH.ExpQ -> TH.ExpQ
trvWrp t =  (\ n e -> if
       | n === TH.mkName "Abs"  ->
           [| case getPrfHasSinArr $(TH.varE t) of
                (PrfHasSin , PrfHasSin) -> $e |]
       | n === TH.mkName "Tpl"  ->
           [| case getPrfHasSinTpl $(TH.varE t) of
                (PrfHasSin , PrfHasSin) -> $e |]
       | n === TH.mkName "Ary"  ->
           [| case getPrfHasSinAry $(TH.varE t) of
                PrfHasSin -> $e |]
       | n === TH.mkName "AryV" ->
           [| case getPrfHasSinVec $(TH.varE t) of
                PrfHasSin -> $e |]
       | n === TH.mkName "Som"  ->
           [| case getPrfHasSinMay $(TH.varE t) of
                PrfHasSin -> $e |]
       | otherwise   -> e)
