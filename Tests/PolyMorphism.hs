import QFeldspar.QDSL
import qualified QFeldspar.Environment.Typed as ET
import qualified QFeldspar.Environment.Scoped as ES
import qualified QFeldspar.Type.GADT as TG
import qualified QFeldspar.Nat.GADT as NG
import QFeldspar.Expression.Utils.TemplateHaskell as TH

iff :: Bool -> a -> a -> a
iff l m n = if l then m else n

-- below is generated automatically by the tool using Template Haskell
et = ET.Ext (TG.Arr TG.Bol (TG.Arr (TG.TVr NG.Zro)
      (TG.Arr (TG.TVr NG.Zro) (TG.TVr NG.Zro)))) ET.Emp

-- below is generated automatically by the tool using Template Haskell
es = ES.Ext (TH.stripNameSpace ('iff)) ES.Emp


testQ1 :: Qt (Bool -> Float)
testQ1 = [|| \ x -> iff x 1.1 1.1 ||]

miniFeldspar1 = translateFWith et es testQ1

{- gives us
    (\ x0 -> (Prm $(nat 0 "") (
     Ext (x0) (
     Ext (ConF (1.1)) (
     Ext (ConF (1.1)) (Emp))))))
   which with some clean ups to make it more human readable it
   would be equivalent to
    (\ x0 -> Prm 0 (x0 <+> ConF 1.1 <+> ConF 1.1 <+> Emp))
   where 0 ranges over the provided environment, representing iff
-}

testQ2 :: Qt Float
testQ2 = [|| (\ x -> iff x 1.1 1.1) True ||]

miniFeldspar2 = translateWith et es testQ2
{- gives us
    Prm $(nat 0 "") (
     Ext (ConB (True)) (
     Ext (ConF (1.1)) (
     Ext (ConF (1.1)) (Emp))))
   which with some clean ups to make it more human readable it
   would be equivalent to
    Prm 0 (ConB True <+> ConF 1.1 <+> ConF 1.1 <+> Emp)
   where 0 ranges over the provided environment, representing iff
-}

-- I am no implementing free evaluator and optimiser
-- (e.g. partial evaluator), which would for example reduce above to
--  ConF 1.1 (based on Haskell semantics)
