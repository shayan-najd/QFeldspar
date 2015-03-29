module QFeldspar.CDSL
  (module QFeldspar.Prelude.MiniFeldspar,shared,(.),
   evaluate,compile,compileF,normalise,normaliseF,
   simplify,simplifyF,cdsl) where

import QFeldspar.Prelude.MiniFeldspar
import QFeldspar.Prelude.Environment
import QFeldspar.Singleton
import QFeldspar.MyPrelude (frmRgt,String,(.))
import qualified QFeldspar.MyPrelude as MP

import QFeldspar.Conversion
import QFeldspar.Expression.Conversions.Evaluation.MiniFeldspar ()
import QFeldspar.Expression.Conversion ()

import qualified QFeldspar.Type.GADT                  as TFG
import qualified QFeldspar.Expression.GADTValue       as FGV
import QFeldspar.Compiler(scompile)
import QFeldspar.Normalisation (nrm)
-- import QFeldspar.ChangeMonad
import QFeldspar.Expression.Utils.Reuse.MiniFeldspar
import QFeldspar.Expression.Utils.MiniFeldspar(shared)
import QFeldspar.CSE(cse,remTag)
import QFeldspar.Simplification (smp)

type C    = String

evaluate :: forall a. Syn a => a -> InT a
evaluate ee = let e = toExp ee
                  FGV.Exp v :: FGV.Exp (InT a)
                    = MP.frmRgtZro (cnv (e , etFGV))
              in v

compile :: forall a. Syn a => Bool -> Bool -> a -> String
compile bSmp bCSE ee = let e = toExp ee
                       in  frmRgt (scompile (sin :: TFG.Typ (InT a))
                                    esString
                                    ((if bSmp then simplify else MP.id)
                                     (normalise bCSE e)))

compileF :: forall a b. (Syn a , Syn b) =>
            Bool -> Bool -> (a -> b) -> String
compileF cSmp cCSE ff = let f = toExpF ff
                    in  frmRgt (scompile (sin :: TFG.Typ (InT b)) esString
                          ((if cSmp then simplifyF else MP.id)
                           (normaliseF cCSE f)))

normalise :: Syn a => Bool -> a -> a
normalise c ee = let e = toExp ee
                 in  frmExp  (onMF (nrm . remTag . (if c then cse else MP.id)) e)


normaliseF :: (Syn a , Syn b) => Bool -> (a -> b) -> a -> b
normaliseF c ff = let f = toExpF ff
                  in  frmExpF (onMFF (nrm . remTag . (if c then cse else MP.id)) f)

simplify :: Syn a => a -> a
simplify = frmExpF (onMF smp)

simplifyF :: (Syn a , Syn b) =>
             (a -> b) -> a -> b
simplifyF ff = let f = toExpF ff
               in  frmExpF (onMFF smp f)

cdsl :: (Type a , Type b) => (Dp a -> Dp b) -> C
cdsl = compileF MP.True MP.True
