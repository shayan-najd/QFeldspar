module QFeldspar.CDSL(module QFeldspar.Prelude.MiniFeldspar
                     ,Dp,evaluate,compile,compileF,normalise,normaliseF
                     ,simplify,simplifyF) where

import QFeldspar.Prelude.MiniFeldspar
import QFeldspar.Prelude.Environment
import QFeldspar.Singleton
import QFeldspar.MyPrelude (frmRgt,String,(.),Bool)
import qualified QFeldspar.MyPrelude as MP

import QFeldspar.Conversion
import QFeldspar.Expression.Conversions.Evaluation.MiniFeldspar ()
import QFeldspar.Expression.Conversion ()

import qualified QFeldspar.Expression.MiniFeldspar  as FMWS
import qualified QFeldspar.Type.GADT                  as TFG
import qualified QFeldspar.Expression.GADTValue       as FGV
import QFeldspar.Compiler(scompile)
import QFeldspar.Normalisation
import QFeldspar.Normalisation.MiniFeldspar ()
import QFeldspar.CSE
import QFeldspar.ChangeMonad
import QFeldspar.Simplify

type Dp a = FMWS.Exp Prelude a

evaluate :: forall a. Syn a => a -> InT a
evaluate ee = let e = toExp ee
                  FGV.Exp v :: FGV.Exp (InT a)
                    = frmRgt (cnv (e , etFGV))
              in v

compile :: forall a. Syn a => MP.Bool -> a -> String
compile c ee = let e = toExp ee
               in  frmRgt (scompile (sin :: TFG.Typ (InT a))
                                    esString
                                    (normalise c e))

compileF :: forall a b. (Syn a , Syn b) => MP.Bool -> (a -> b) -> String
compileF c ff = let f = toExpF ff
                in  frmRgt (scompile (sin :: TFG.Typ (InT b)) esString
                                     (normaliseF c f))

normalise :: Syn a => Bool -> a -> a
normalise c ee = let e = toExp ee
                 in  frmExp (nrm (if c then cse e else remTag e))

normaliseF :: (Syn a , Syn b) => Bool -> (a -> b) -> a -> b
normaliseF c ff = let f = toExpF ff
                  in  frmExpF (nrm (if c then cseF f else remTag . f))

simplify :: Syn a => a -> a
simplify = frmExpF (tilNotChg smpOne)

simplifyF :: (Syn a , Syn b) =>
             (a -> b) -> a -> b
simplifyF ff = let f = toExpF ff
               in  frmExpF (tilNotChg smpOne f)
