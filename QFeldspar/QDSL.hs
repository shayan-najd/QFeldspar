{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing #-}
module QFeldspar.QDSL
  (Qt,FO,Type,Num,Eq,Ord,
   Int,Float,Bool(..),while,fst,snd,Ary,mkArr,lnArr,ixArr,
   Vec(..),Complex(..),Maybe(..),(*),(+),(-),(==),(<),save,
   realPart,imagPart,div,(/),(.&.),(.|.),xor,shfRgt,shfLft,
   complement,i2f,cis,ilog2,sqrt,hashTable,
   maybe,return,(>>=),(.),
   qdsl,evaluate,translate,translateF,compile,compileF,
   dbg,dbgF,testQt,testNrmQt,testNrmSmpQt,testDpF,toDp,wrp,
   ghoF{-,nghoF-},gho{-,ngho-},trmEql) where

import QFeldspar.MyPrelude hiding (while,save)
import qualified QFeldspar.MyPrelude as MP

import QFeldspar.Expression.Utils.Show.GADTFirstOrder()
import QFeldspar.Expression.Utils.Show.GADTHigherOrder()
import QFeldspar.Expression.Utils.Show.MiniFeldspar()

import QFeldspar.CDSL (Dp)
import qualified QFeldspar.CDSL as CDSL

import QFeldspar.Singleton

import QFeldspar.Expression.Utils.TemplateHaskell
    (trmEql,stripNameSpace)

import QFeldspar.Conversion
import QFeldspar.Expression.Conversion ()
import QFeldspar.Expression.Conversions.Evaluation.MiniFeldspar ()
import QFeldspar.Expression.Conversions.Lifting(cnvFOHOF)

import qualified QFeldspar.Expression.ADTUntypedNamed as FAUN
import qualified QFeldspar.Expression.Utils.ADTUntypedNamed as FAUN
import qualified QFeldspar.Expression.GADTHigherOrder as FGHO
import qualified QFeldspar.Expression.GADTFirstOrder as GFO
import qualified Language.Haskell.TH.Syntax as TH

import qualified QFeldspar.Type.GADT as TFG
import qualified QFeldspar.Normalisation as GFO

import QFeldspar.Prelude.Environment (etTFG,(<:>))
import qualified QFeldspar.Prelude.HaskellEnvironment as PHE

type Data a = TH.Q (TH.TExp a)
type Qt a = Data a
type C    = String
type Type a = HasSin TFG.Typ a

class    FO a                              where {}
instance FO MP.Bol                         where {}
instance FO MP.Int                         where {}
instance FO MP.Flt                         where {}
instance (FO a , FO b) => FO (a , b)       where {}
instance FO a => FO (MP.Ary a)             where {}
instance FO MP.Cmx                         where {}

instance TH.Lift MP.Int where
  lift i = MP.return (TH.LitE (TH.IntegerL (MP.toInteger i)))

instance TH.Lift MP.Flt where
  lift f = MP.return (TH.LitE (TH.RationalL (MP.toRational f)))

while :: FO a => (a -> MP.Bol) -> (a -> a) -> a -> a
while = MP.while

save :: FO a => a -> a
save = MP.save

dn :: TH.Name
dn = (TH.Name (TH.OccName "dummyy") TH.NameS)

dummy :: Data a
dummy = MP.return (TH.TExp (TH.VarE dn))

wrp :: Type a => Data a -> FAUN.Exp TH.Name
wrp = expand
        ['(>>=)      := [|| \m -> \k ->
                              case m of
                              {Nothing -> Nothing ; Just x -> k x} ||],
         'maybe      := [|| \x -> \g -> \m ->
                               case m of
                               {Nothing -> x ; Just y -> g y} ||],
         'return     := [|| \x -> Just x ||],
         '(.)        := [|| \f -> \g -> \x -> f (g x) ||],
         'realPart   := [|| \x -> PHE.realPart x ||],
         'imagPart   := [|| \x -> PHE.imagPart x ||],
         'div        := [|| \x -> \y -> PHE.divInt x y ||],
         '(/)        := [|| \x -> \y -> PHE.divFlt x y ||],
         '(.&.)      := [|| \x -> \y -> PHE.andInt x y ||],
         '(.|.)      := [|| \x -> \y -> PHE.orInt  x y ||],
         'xor        := [|| \x -> \y -> PHE.xorInt x y ||],
         'shfRgt     := [|| \x -> \y -> PHE.shrInt x y ||],
         'shfLft     := [|| \x -> \y -> PHE.shlInt x y ||],
         'complement := [|| \x -> PHE.cmpInt   x ||],
         'i2f        := [|| \x -> PHE.i2f  x ||],
         'cis        := [|| \x -> PHE.cis      x ||],
         'ilog2      := [|| \x -> PHE.ilog2    x ||],
         'sqrt       := [|| \x -> PHE.sqrtFlt  x ||],
         'hashTable  := [|| PHE.hshTbl ||]]
        . wrpTyp

wrpTyp :: forall a. Type a => Data a -> Data a
wrpTyp ee = do e <- ee
               return (TH.TExp (TH.SigE (TH.unType e)
                         (frmRgtZro (cnv (sin :: TFG.Typ a , ())))))

translate :: forall a.
             (Type a , FO a) =>
             Qt a -> Dp a
translate f = frmRgtZro (cnv (wrp f , etTFG , PHE.esTH))

translateF :: forall a b.
             (Type a , Type b) =>
             Qt (a -> b) -> Dp a -> Dp b
translateF f = let e :: GFO.Exp (a ': PHE.Prelude) b =
                    frmRgtZro (cnv (wrp
                      ([|| $$f $$dummy ||])
                      , (sin :: TFG.Typ a) <:> etTFG, dn PHE.<+> PHE.esTH))
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
dbg e = frmRgtZro (cnv(wrp e,etTFG , PHE.esTH))

dbgF :: (Type a , Type b) => Qt (a -> b) -> FAUN.Exp TH.Name
dbgF e = frmRgtZro (cnv(wrp e,etTFG , PHE.esTH))

gho :: Type a => Qt a -> FGHO.Exp PHE.Prelude a
gho e = frmRgtZro (cnv(wrp e,etTFG , PHE.esTH))

ghoF :: (Type a , Type b) =>
        Qt (a -> b) -> FGHO.Exp PHE.Prelude (a -> b)
ghoF e = frmRgtZro (cnv(wrp e,etTFG , PHE.esTH))

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
