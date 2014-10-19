module QFeldspar.QDSL (module QFeldspar.Prelude.TemplateHaskell
                      ,Qt,translate,translateF,evaluate,compile,compileF)
    where

import QFeldspar.CDSL (Dp)
import qualified QFeldspar.CDSL as CDSL

import QFeldspar.Prelude.TemplateHaskell

import QFeldspar.Prelude.Environment
import QFeldspar.Singleton
import QFeldspar.MyPrelude (frmRgt,(.),String)

import QFeldspar.Conversion
import QFeldspar.Expression.Feldspar.Conversions.Evaluation.MiniWellScoped ()
import QFeldspar.Expression.Feldspar.Conversion ()

import qualified QFeldspar.Expression.Feldspar.MiniWellScoped  as FMWS
import qualified QFeldspar.Type.Feldspar.GADT                  as TFG

type Qt a = Data a
type C    = String

dummy :: a
dummy = dummy

translate :: forall a.
             (HasSin TFG.Typ a , FO a) =>
             Qt a -> Dp a
translate f = frmRgt (cnv (f , etTFG , esTH))

translateF :: forall a b.
             (HasSin TFG.Typ a , HasSin TFG.Typ b) =>
             Qt (a -> b) -> Dp a -> Dp b
translateF f x = FMWS.absVar
                          (frmRgt
                           (cnv ([|| $$f dummy  ||]
                                , (sin :: TFG.Typ a) <:> etTFG
                                , 'dummy <+> esTH))) x

evaluate ::  forall a.
             (HasSin TFG.Typ a , FO a) =>
             Qt a -> a
evaluate = CDSL.evaluate . translate

compile :: forall a.
             (HasSin TFG.Typ a, FO a) =>
             Bool -> Qt a -> C
compile b = CDSL.compile b . translate

compileF :: forall a b.
             (HasSin TFG.Typ a , HasSin TFG.Typ b , FO a) =>
             Bool -> Qt (a -> b) -> C
compileF b = CDSL.compileF b . translateF