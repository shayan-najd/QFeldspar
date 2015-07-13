module QFeldspar.CDSL
  (module QFeldspar.Prelude.MiniFeldspar{-,shared-},(.),
   evaluate,compile,compileF,normalise,normaliseF,compileF',
   simplify,simplifyF,cdsl,makeIP,makeIPAt) where

import QFeldspar.Prelude.MiniFeldspar
import QFeldspar.Prelude.Environment
import QFeldspar.Singleton
import QFeldspar.MyPrelude (frmRgt,String,(.),(++),IO,writeFile,return)
import qualified QFeldspar.MyPrelude as MP

import QFeldspar.Conversion
import QFeldspar.Expression.Conversions.Evaluation.MiniFeldspar ()
import QFeldspar.Expression.Conversion ()

import qualified QFeldspar.Type.GADT                  as TG
import qualified QFeldspar.Expression.GADTValue       as FGV
import QFeldspar.Compiler(scompileWith',scompile)
import QFeldspar.Normalisation (nrm)
-- import QFeldspar.ChangeMonad
import QFeldspar.Expression.Utils.Reuse.MiniFeldspar
-- import QFeldspar.Expression.Utils.MiniFeldspar(shared)
import QFeldspar.CSE(cse,remTag)
import QFeldspar.Simplification (smp)
import System.Process
import qualified Language.Haskell.TH.Syntax as TH

type C    = String

evaluate :: forall a. Syn a => a -> InT a
evaluate ee = let e = toExp ee
                  FGV.Exp v :: FGV.Exp (InT a)
                    = MP.frmRgtZro (cnv (e , etFGV))
              in v

compile :: forall a. Syn a => Bool -> Bool -> a -> String
compile bSmp bCSE ee = let e = toExp ee
                       in  frmRgt (scompile (sin :: TG.Typ (InT a))
                                    esString
                                    ((if bSmp then simplify else MP.id)
                                     (normalise bCSE e)))

compileF :: forall a b.
            (Syn a , Syn b) =>
            Bool -> Bool -> (a -> b) -> String
compileF = compileF' MP.True

compileF' :: forall a b.
             (Syn a , Syn b) =>
             Bool -> Bool -> Bool -> (a -> b) -> String
compileF' b cSmp cCSE ff = let f = toExpF ff
                           in  frmRgt (scompileWith' b [] (sin :: TG.Typ (InT b)) esString 0
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

makeIP :: String -> String -> IO ()
makeIP = makeIPAt "./Examples/C/"

makeIPAt :: String -> String -> String -> IO ()
makeIPAt path c name  = do
  let fileContent = $(do f1 <- TH.runIO (MP.readFile "QFeldspar/ppm.h")
                         f2 <- TH.runIO (MP.readFile "QFeldspar/header.h")
                         TH.lift (f1 ++"\n" ++ f2))
                     ++ "\n" ++
                    c
                    ++
                    "\nint main(int argc, char *argv[])"++
                    "\n{"++
                    "\n  Image   imgIn = readImage(argv[1]);"++
                    "\n  AryWrd aryIn = newAry(Wrd,size(imgIn)); "++
                    "\n  for (unsigned int i = 0; i < size(imgIn); i++)"++
                    "\n    aryIn = setAry(aryIn , i , imgIn.data[i]);"++
                    "\n  AryWrd aryOut;"++
                    "\n  aryOut = func(aryIn);"++
                    "\n  unsigned int t;"++
                    "\n  sscanf(argv[3],\"%d\",&t);" ++
                    "\n  Image imgOut = {.sizeX = imgIn.sizeX, "++
                    "\n                  .sizeY = imgIn.sizeY,"++
                    "\n                  .type  = t,"++
                    "\n                  .data  = malloc(len(aryOut) * (t == 3 ? 3 : 1) * sizeof(unsigned int))}; "++
                    "\n  for(unsigned int i = 0; i < size(imgOut); i++)"++
                    "\n    imgOut.data[i] = ind(aryOut , i);"++
                    "\n  writeImage (argv[2] , imgOut);"++
                    "\n  return 0;"++
                    "\n}"
     in do writeFile (path ++ "/" ++ name++".c") fileContent
           _ <- runCommand ("gcc -o " ++ path ++ "/" ++ name ++" " ++ path ++ "/" ++ name ++ ".c -lm -std=c99")
           return ()
