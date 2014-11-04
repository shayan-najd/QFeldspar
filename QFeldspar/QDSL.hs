{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing #-}
module QFeldspar.QDSL (module TH
                      ,Qt,translate,translateF,evaluate,compile,compileF,wrp)
    where

import QFeldspar.CDSL (Dp)
import qualified QFeldspar.CDSL as CDSL

import QFeldspar.Prelude.TemplateHaskell as TH

import QFeldspar.Prelude.Environment
import QFeldspar.Singleton
import QFeldspar.MyPrelude (frmRgt,(.),String,Maybe)
import qualified QFeldspar.MyPrelude as MP

import QFeldspar.Conversion
import QFeldspar.Expression.Feldspar.Conversions.Evaluation.MiniFeldspar ()
import QFeldspar.Expression.Feldspar.Conversion ()

import qualified QFeldspar.Expression.Feldspar.MiniFeldspar  as FMWS
import qualified QFeldspar.Type.Feldspar.GADT                as TFG

import QFeldspar.Type.Feldspar.Conversion ()

import qualified Language.Haskell.TH.Syntax as TH

type Qt a = Data a
type C    = String
type Type a = HasSin TFG.Typ a

dn :: TH.Name
dn = (TH.Name (TH.OccName "dummyy") TH.NameS)

dummy :: Data a
dummy = MP.return (TH.TExp (TH.VarE dn))

wrp :: Type a => Data a -> Data a
wrp f = wrpTyp [|| let (==)   = (\ x -> \ y -> eqlFltHsk x y) in
                   let (/)    = (\ x -> \ y -> divFltHsk x y) in
                   let (*)    = (\ x -> \ y -> mulFltHsk x y) in
                   let return = $$(TH.return) :: (Flt -> Maybe Flt) in
                   let _bnd   = ($$bind)
                          :: (Maybe Flt -> (Flt -> Maybe Flt) -> Maybe Flt) in
                   let maybe  = $$(TH.maybe)
                          :: (Flt -> (Flt -> Flt) -> Maybe Flt -> Flt) in
                   $$f ||]

wrpTyp :: forall a. Type a => Data a -> Data a
wrpTyp ee = do e <- ee
               MP.return (TH.TExp (TH.SigE (TH.unType e)
                                  (frmRgt (cnv (sin :: TFG.Typ a , ())))))

translate :: forall a.
             (Type a , FO a) =>
             Qt a -> Dp a
translate f = frmRgt (cnv (wrp f , etTFG , esTH))

translateF :: forall a b.
             (Type a , Type b) =>
             Qt (a -> b) -> Dp a -> Dp b
translateF f x = FMWS.absVar
                 (frmRgt
                 (cnv (wrp
                 ([|| $$f $$dummy ||])
                 , (sin :: TFG.Typ a) <:> etTFG
                 , dn <+> esTH))) x

evaluate ::  forall a.
             (Type a , FO a) =>
             Qt a -> a
evaluate = CDSL.evaluate . translate . wrp

compile :: forall a.
             (Type a, FO a) =>
             Bool -> Qt a -> C
compile b = CDSL.compile b . translate . wrp

compileF :: forall a b.
             (Type a , Type b , FO a) =>
             Bool -> Qt (a -> b) -> C
compileF b = CDSL.compileF b . translateF . wrp