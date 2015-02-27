{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing #-}
module QFeldspar.QDSL
    (module TH,dbg,dbgF,Type,testQt,testNrmQt,testNrmSmpQt,testDpF,toDp
    ,Qt,translate,translateF,evaluate,compile,compileF,wrp
    ,ghoF{-,nghoF-},gho{-,ngho-},qdsl,trmEql
    ,realPartHsk
    ,imagPartHsk
    ,divIntHsk
    ,divFltHsk
    ,divCmxHsk
    ,andIntHsk
    ,orIntHsk
    ,xorIntHsk
    ,shrIntHsk
    ,shlIntHsk
    ,cmpIntHsk
    ,i2fHsk
    ,cisHsk
    ,ilog2Hsk
    ,sqrtFltHsk
    ,hshTblHsk)
    where
import QFeldspar.Expression.Utils.Show.GADTFirstOrder()
import QFeldspar.Expression.Utils.Show.GADTHigherOrder()
import QFeldspar.Expression.Utils.Show.MiniFeldspar()

import Prelude(Bool(..),String,(.))
import QFeldspar.CDSL (Dp)
import qualified QFeldspar.CDSL as CDSL

import QFeldspar.Prelude.TemplateHaskell as TH

import QFeldspar.Singleton
import QFeldspar.MyPrelude (frmRgtZro)
import qualified QFeldspar.MyPrelude as MP
import QFeldspar.Expression.Utils.TemplateHaskell
    (trmEql,stripNameSpace)

import QFeldspar.Conversion
import QFeldspar.Expression.Conversions.Evaluation.MiniFeldspar ()
import QFeldspar.Expression.Conversion ()
import QFeldspar.Expression.Conversions.Lifting(cnvFOHOF)

import qualified QFeldspar.Expression.ADTUntypedNamed as FAUN
import qualified QFeldspar.Expression.Utils.ADTUntypedNamed as FAUN
import qualified QFeldspar.Expression.GADTHigherOrder as FGHO
import qualified QFeldspar.Expression.GADTFirstOrder as GFO
import qualified QFeldspar.Type.GADT                  as TFG

import QFeldspar.Type.Conversion ()
import qualified QFeldspar.Normalisation as GFO
import qualified Language.Haskell.TH.Syntax as TH
import QFeldspar.Prelude.HaskellEnvironment
import QFeldspar.Prelude.Environment

type Qt a = Data a
type C    = String
type Type a = HasSin TFG.Typ a

dn :: TH.Name
dn = (TH.Name (TH.OccName "dummyy") TH.NameS)

dummy :: Data a
dummy = MP.return (TH.TExp (TH.VarE dn))

_bnd :: a
_bnd = _bnd

wrp :: Type a => Data a -> FAUN.Exp TH.Name
wrp = expand
        ['_bnd      := bnd,
         'MP.maybe  := may,
         'MP.return := ret,
         '(MP./)    := [|| \x -> \y -> divFltHsk x y ||],
         '(MP..)    := [|| \f -> \g -> \x -> f (g x) ||],
         'MP.sqrt   := [|| \x -> sqrtFltHsk x ||]]
        . wrpTyp

wrpTyp :: forall a. Type a => Data a -> Data a
wrpTyp ee = do e <- ee
               MP.return (TH.TExp (TH.SigE (TH.unType e)
                                  (frmRgtZro (cnv (sin :: TFG.Typ a , ())))))

translate :: forall a.
             (Type a , FO a) =>
             Qt a -> Dp a
translate f = frmRgtZro (cnv (wrp f , etTFG , esTH))

translateF :: forall a b.
             (Type a , Type b) =>
             Qt (a -> b) -> Dp a -> Dp b
translateF f = let e :: GFO.Exp (a ': Prelude) b =
                        frmRgtZro (cnv (wrp
                                        ([|| $$f $$dummy ||])
                                       , (sin :: TFG.Typ a) <:> etTFG, dn <+> esTH))
               in frmRgtZro (cnv (cnvFOHOF etTFG (GFO.nrm e) , ()))

evaluate ::  forall a.
             (Type a , FO a) =>
             Qt a -> a
evaluate = CDSL.evaluate . translate

compile :: forall a.
             (Type a, FO a) =>
             Bool -> Bool -> Qt a -> C
compile b1 b2 = CDSL.compile b1 b2 . translate

compileF :: forall a b.
             (Type a , Type b , FO a) =>
             Bool -> Bool -> Qt (a -> b) -> C
compileF b1 b2 = CDSL.compileF b1 b2 . translateF

dbg :: Type a => Qt a -> FAUN.Exp TH.Name
dbg e = frmRgtZro (cnv(wrp e,etTFG , esTH))

dbgF :: (Type a , Type b) => Qt (a -> b) -> FAUN.Exp TH.Name
dbgF e = frmRgtZro (cnv(wrp e,etTFG , esTH))

gho :: Type a => Qt a -> FGHO.Exp Prelude a
gho e = frmRgtZro (cnv(wrp e,etTFG , esTH))

ghoF :: (Type a , Type b) => Qt (a -> b) -> FGHO.Exp Prelude (a -> b)
ghoF e = frmRgtZro (cnv(wrp e,etTFG , esTH))

-- nghoF :: (Type a , Type b) => Qt (a -> b) -> FGHO.Exp Prelude (a -> b)
-- nghoF e = nrm (ghoF e)

-- ngho :: Type a => Qt a -> FGHO.Exp Prelude a
-- ngho e = nrm (gho e)

qdsl :: (FO a , Type a , Type b) => Qt (a -> b) -> C
qdsl = compileF True True


-- For paper
testQt :: Qt a -> Qt a -> Bool
testQt = trmEql

toDp :: (Type a , Type b) => Qt (a -> b) -> Dp a -> Dp b
toDp = translateF

testNrmQt :: (Type a , Type b) => Qt (a -> b) -> Qt (a -> b) -> Bool
testNrmQt x y = testDpF (toDp x) (toDp y)

testNrmSmpQt :: (Type a , Type b) => Qt (a -> b) -> Qt (a -> b) -> Bool
testNrmSmpQt x y = testDpF (CDSL.simplifyF (toDp x)) (CDSL.simplifyF (toDp y))


testDpF :: (Type a , Type b) => (Dp a -> Dp b) -> (Dp a -> Dp b) -> Bool
testDpF = CDSL.trmEqlF

toFAUN :: Qt a -> MP.ErrM (FAUN.Exp TH.Name)
toFAUN ee = MP.evalStateT
            (do ee' :: TH.Exp <- MP.lift (TH.runQ (TH.unTypeQ ee))
                cnv (ee',())) 0

data Sbs where
  (:=) :: TH.Name -> Qt a -> Sbs

expand :: [Sbs] -> Qt a -> FAUN.Exp TH.Name
expand sbs ee = MP.frmRgt
                (do ee' <- toFAUN ee
                    MP.foldM
                     (\ e (n := es) -> do es' <- toFAUN es
                                          MP.return (FAUN.sbs (stripNameSpace n) es' e))
                     ee' sbs)
