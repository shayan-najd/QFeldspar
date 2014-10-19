module QFeldspar.CDSL(module QFeldspar.Prelude.MiniWellScoped
                     ,Dp,evaluate,compile,compileF) where

import QFeldspar.Prelude.MiniWellScoped
import QFeldspar.Prelude.Environment
import QFeldspar.Singleton
import QFeldspar.MyPrelude (frmRgt,String,(.))
import qualified QFeldspar.MyPrelude as MP

import QFeldspar.Conversion
import QFeldspar.Expression.Feldspar.Conversions.Evaluation.MiniWellScoped ()
import QFeldspar.Expression.Feldspar.Conversion ()

import qualified QFeldspar.Expression.Feldspar.MiniWellScoped  as FMWS
import qualified QFeldspar.Type.Feldspar.GADT                  as TFG
import qualified QFeldspar.Expression.Feldspar.GADTValue       as FGV
import QFeldspar.Compiler(scompile)
import QFeldspar.Normalization
import QFeldspar.Normalization.Feldspar.MiniWellscoped ()
import QFeldspar.CSE


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
                               (nrm (if c then cse e else remTag e)))
              in  es

compileF :: forall a b.
            (HasSin TFG.Typ a,HasSin TFG.Typ b) =>
            MP.Bool ->
            (Dp a -> Dp b) -> String
compileF c f = let fs = frmRgt (scompile (sin :: TFG.Typ b) esString
                               (nrm (if c then cseF f else remTag . f)))
               in  fs
