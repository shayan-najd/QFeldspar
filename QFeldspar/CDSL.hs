module QFeldspar.CDSL(module QFeldspar.Prelude.MiniFeldspar
                     ,Dp,evaluate,compile,compileF,normalise,normaliseF
                     ,simplify,simplifyF) where

import QFeldspar.Prelude.MiniFeldspar
import QFeldspar.Prelude.Environment
import QFeldspar.Singleton
import QFeldspar.MyPrelude (frmRgt,String,(.))
import qualified QFeldspar.MyPrelude as MP

import QFeldspar.Conversion
import QFeldspar.Expression.Feldspar.Conversions.Evaluation.MiniFeldspar ()
import QFeldspar.Expression.Feldspar.Conversion ()

import qualified QFeldspar.Expression.Feldspar.MiniFeldspar  as FMWS
import qualified QFeldspar.Type.Feldspar.GADT                  as TFG
import qualified QFeldspar.Expression.Feldspar.GADTValue       as FGV
import QFeldspar.Compiler(scompile)
import QFeldspar.Normalisation
import QFeldspar.Normalisation.Feldspar.MiniFeldspar ()
import QFeldspar.CSE
import QFeldspar.ChangeMonad
import QFeldspar.Simplify

type Dp a = FMWS.Exp Prelude a

evaluate :: forall a. HasSin TFG.Typ a => Dp a -> a
evaluate ee = let (FGV.Exp e) :: FGV.Exp a
                              = frmRgt (cnv (ee , etFGV))
              in e

compile :: forall a.
            (HasSin TFG.Typ a) =>
            MP.Bool ->
            Dp a -> String
compile c e = let es = frmRgt (scompile (sin :: TFG.Typ a)
                                              esString
                               (normalise c e))
              in  es

compileF :: forall a b.
            (HasSin TFG.Typ a,HasSin TFG.Typ b) =>
            MP.Bool ->
            (Dp a -> Dp b) -> String
compileF c f = let fs = frmRgt (scompile (sin :: TFG.Typ b) esString
                                (normaliseF c f))
               in  fs

normalise :: HasSin TFG.Typ t =>
             Bool -> FMWS.Exp r t -> FMWS.Exp r t
normalise c e = nrm (if c then cse e else remTag e)

normaliseF :: (HasSin TFG.Typ b, HasSin TFG.Typ a) =>
              Bool -> (FMWS.Exp r a -> FMWS.Exp r b) ->
                       FMWS.Exp r a -> FMWS.Exp r b
normaliseF c f = nrm (if c then cseF f else remTag . f)

simplify :: HasSin TFG.Typ t =>
            FMWS.Exp r t -> FMWS.Exp r t
simplify = tilNotChg smpOne

simplifyF :: (HasSin TFG.Typ b, HasSin TFG.Typ a) =>
             (FMWS.Exp r a -> FMWS.Exp r b) ->
             FMWS.Exp r a -> FMWS.Exp r b
simplifyF = tilNotChg smpOne