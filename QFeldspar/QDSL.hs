{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing #-}
module QFeldspar.QDSL (module TH,dbg,dbgF
                      ,Qt,translate,translateF,evaluate,compile,compileF,wrp
                      ,ghoF,nghoF,gho,ngho)
    where

import QFeldspar.CDSL (Dp)
import qualified QFeldspar.CDSL as CDSL

import QFeldspar.Prelude.TemplateHaskell as TH

import QFeldspar.Singleton
import QFeldspar.MyPrelude (frmRgt,(.),String,Maybe)
import qualified QFeldspar.MyPrelude as MP

import QFeldspar.Conversion
import QFeldspar.Expression.Feldspar.Conversions.Evaluation.MiniFeldspar ()
import QFeldspar.Expression.Feldspar.Conversion ()

import qualified QFeldspar.Expression.Feldspar.ADTUntypedNamed as FAUN
import qualified QFeldspar.Expression.Feldspar.GADTHigherOrder as FGHO
import qualified QFeldspar.Expression.Feldspar.MiniFeldspar    as FMWS
import qualified QFeldspar.Type.Feldspar.GADT                  as TFG

import QFeldspar.Type.Feldspar.Conversion ()

import qualified Language.Haskell.TH.Syntax as TH
import QFeldspar.Normalisation

type Qt a = Data a
type C    = String
type Type a = HasSin TFG.Typ a

dn :: TH.Name
dn = (TH.Name (TH.OccName "dummyy") TH.NameS)

dummy :: Data a
dummy = MP.return (TH.TExp (TH.VarE dn))

wrp :: Type a => Data a -> Data a
wrp f = wrpTyp [|| let (**)   = (\ x -> \ y -> mulIntHsk x y)       in
                   let (<)    = (\ x -> \ y -> ltdIntHsk x y)       in
                   let (+)    = (\ x -> \ y -> addIntHsk x y)       in
                   let (==)   = (\ x -> \ y -> eqlFltHsk x y)       in
                   let (/)    = (\ x -> \ y -> divFltHsk x y)       in
                   let (*)    = (\ x -> \ y -> mulFltHsk x y)       in
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

dbg :: Type a => Qt a -> FAUN.Exp TH.Name
dbg e = frmRgt (cnv(wrp e,etTFG , esTH))

dbgF :: (Type a , Type b) => Qt (a -> b) -> FAUN.Exp TH.Name
dbgF e = frmRgt (cnv(wrp e,etTFG , esTH))

gho :: Type a => Qt a -> FGHO.Exp Prelude a
gho e = frmRgt (cnv({-wrp-} e,etTFG , esTH))

ghoF :: (Type a , Type b) => Qt (a -> b) -> FGHO.Exp Prelude (a -> b)
ghoF e = frmRgt (cnv(wrp e,etTFG , esTH))

nghoF :: (Type a , Type b) => Qt (a -> b) -> FGHO.Exp Prelude (a -> b)
nghoF e = nrm (ghoF e)

ngho :: Type a => Qt a -> FGHO.Exp Prelude a
ngho e = nrm (gho e)
