{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module QFeldspar.Compiler where

import QFeldspar.MyPrelude hiding (fst,snd)

import QFeldspar.Expression.C
import qualified QFeldspar.Expression.MiniFeldspar as FMWS

import qualified QFeldspar.Type.ADT as TFA
import qualified QFeldspar.Type.GADT as TFG

import qualified QFeldspar.Environment.Scoped as ES
import qualified QFeldspar.Environment.Typed as ET

import qualified QFeldspar.Variable.Scoped as VS

import QFeldspar.Conversion
import QFeldspar.Type.Conversion ()
import QFeldspar.Variable.Conversion ()
import QFeldspar.Expression.Conversions.CodeGeneration (pretty)
import QFeldspar.Expression.Utils.Common
import QFeldspar.Singleton

type CompileMonad a = StateT (Word32,[Var],[Var]) ErrM a

newName :: CompileMonad String
newName = do
 (i , ps , vs) <- getState
 put (i+1 , ps , vs)
 return ("v" ++ (show i))

addParam :: Var -> CompileMonad ()
addParam p = do
  (i , ps , vs) <- getState
  put (i , p:ps , vs)

addVar :: Var -> CompileMonad ()
addVar v = do
  (i , ps , vs) <- getState
  put (i , ps , v:vs)

runCompileMonad :: TFA.Typ -> CompileMonad (Exp , [Stmt]) -> Word32 ->
                   ErrM Func
runCompileMonad ty m i = do ((exp,stmts),(_,ps,vs)) <- runStateT m (i,[],[])
                            pure (Func ty "func" ps
                                  ([Declare (v , t) | (v , t) <- vs] ++
                                   stmts ++ [Return exp]))

cnvImpLft :: (Cnv (a, r) b, ?r :: r) =>
             a -> CompileMonad b
cnvImpLft  = lift . runNamM . cnvImp

cmpImp :: (Compilable (t, r), ?r :: r) =>
          t -> CompileMonad (Exp , [Stmt])
cmpImp e = compile (e , ?r)

prm0 :: String -> Exp
prm0         = Var

prm1 :: String -> Exp -> Exp
prm1 x l     = App x [l]

prm2 :: String -> Exp -> Exp -> Exp
prm2 x l m   = App x [l , m]

prm3 :: String -> Exp -> Exp -> Exp -> Exp
prm3 x l m n = App x [l , m , n]

fls :: Exp
fls = prm0 "false"

tru :: Exp
tru = prm0 "true"

fst :: Exp -> Exp
fst = prm1 "fst"

snd :: Exp -> Exp
snd = prm1 "snd"

len :: Exp -> Exp
len = prm1 "len"

ind :: Exp -> Exp -> Exp
ind = prm2 "ind"

setAry :: Exp -> Exp -> Exp -> Exp
setAry = prm3 "setAry"

cmx :: Exp -> Exp -> Exp
cmx = prm2 "cmx"

mul :: Exp -> Exp -> Exp
mul = prm2 "mul"

add :: Exp -> Exp -> Exp
add = prm2 "add"

sub :: Exp -> Exp -> Exp
sub = prm2 "sub"

eql :: Exp -> Exp -> Exp
eql = prm2 "eql"

ltd :: Exp -> Exp -> Exp
ltd = prm2 "ltd"

newTpl :: TFA.Typ -> Exp -> Exp -> Exp
newTpl (TFA.Tpl tf ts) ef es = App "newTpl"
                               [Var (show (pretty tf) ++ show (pretty ts)) , ef , es]
newTpl _ _ _  = impossible

newAry :: TFA.Typ -> Exp -> Exp
newAry (TFA.Ary t) e  = App "newAry" [Var (show (pretty t)) , e]
newAry _           _  = impossible

class Compilable t where
 compile :: t -> CompileMonad (Exp , [Stmt])

instance (HasSin TFG.Typ t, n ~ Len r) =>
         Compilable (FMWS.Exp r t, ES.Env n String) where
  compile (ee , r) = let ?r = r in do
    let t = sin :: TFG.Typ t
    t'  <- cnvImpLft t
    case ee of
      FMWS.Tag _ e       -> cmpImp e
      FMWS.Mem e         -> cmpImp e
      FMWS.Tmp  x        -> return (Var x , [])
      FMWS.ConI i        -> return (Wrd i , [])
      FMWS.ConB True     -> return (tru   , [])
      FMWS.ConB False    -> return (fls   , [])
      FMWS.ConF f        -> return (Flt f , [])
      FMWS.Prm  v ET.Emp -> do v' :: VS.Var n <- cnvImpLft v
                               let ve = ES.get v' r
                               return (Var ve , [])
      FMWS.Prm  v es     -> do v' :: VS.Var n <- cnvImpLft v
                               let ve         = ES.get v' r
                               (es' , ss)     <- cnvETEnv (sinTypOf v t) es
                               return (App ve es' ,concat ss)
      FMWS.Cnd ec et ef  -> do (ec' , sc) <- cmpImp ec
                               (et' , st) <- cmpImp et
                               (ef' , sf) <- cmpImp ef
                               v <- newName
                               addVar (v , t')
                               return (Var v
                                      , sc ++
                                        [If ec'
                                          (st ++ [Assign v et'])
                                          (sf ++ [Assign v ef'])])
      FMWS.Whl ec eb ei -> do xs <- newName
                              addVar (xs , t')
                              (ec' , sc) <- cmpImp (ec (FMWS.Tmp xs))
                              (eb' , sb) <- cmpImp (eb (FMWS.Tmp xs))
                              (ei' , si) <- cmpImp ei
                              return ( Var xs
                                     , si ++
                                       [Assign xs ei'] ++
                                       sc ++
                                       [Whl ec'
                                        (sb ++
                                         [Assign xs eb'] ++ sc)])
      FMWS.Tpl ef es           -> case TFG.getPrfHasSinTpl t of
       (PrfHasSin , PrfHasSin) -> do (ef' , sf) <- cmpImp ef
                                     (es' , ss) <- cmpImp es
                                     return (newTpl t' ef' es', sf ++ ss)
      FMWS.Fst e               -> do (e'  , se) <- cmpImp e
                                     return (fst e' , se)
      FMWS.Snd e               -> do (e' , se) <- cmpImp e
                                     return (snd e' , se)
      FMWS.Ary l f             -> case TFG.getPrfHasSinAry t of
        PrfHasSin              -> do xl <- newName
                                     addVar (xl , TFA.Wrd)
                                     xa <- newName
                                     addVar (xa , t')
                                     xi <- newName
                                     addVar (xi , TFA.Wrd)
                                     (el , sl) <- cmpImp l
                                     (ef , sf) <- cmpImp (f (FMWS.Tmp xi))
                                     return ( Var xa
                                            ,   sl ++
                                              [ Assign xl el
                                              , Assign xa (newAry t' (Var xl))
                                              , Assign xi (Wrd 0)
                                              , Whl (ltd (Var xi) (Var xl))
                                                (sf ++
                                                 [ Assign xa (setAry (Var xa)
                                                               (Var xi) ef)
                                                 , Assign xi (add (Var xi)
                                                              (Wrd 1))])])
      FMWS.Len e               -> do (e'  , se) <- cmpImp e
                                     return (len e' , se)
      FMWS.Ind ea ei           -> do (ea' , sa) <- cmpImp ea
                                     (ei' , si) <- cmpImp ei
                                     return (ind ea' ei' , sa ++ si)
      FMWS.LeT el eb           -> do xl <- newName
                                     tl <- cnvImpLft (sinTypOf el t)
                                     addVar (xl , tl)
                                     (el' , sl) <- cmpImp el
                                     (eb' , sb) <- cmpImp (eb (FMWS.Tmp xl))
                                     return (eb' , sl ++
                                                   [Assign xl el'] ++
                                                   sb)
      FMWS.Cmx er ei           -> do (er' , sr) <- cmpImp er
                                     (ei' , si) <- cmpImp ei
                                     return (cmx er' ei' , sr ++ si)
      FMWS.Mul er ei           -> do (er' , sr) <- cmpImp er
                                     (ei' , si) <- cmpImp ei
                                     case t of
                                       TFG.Wrd -> return (mul er' ei' , sr ++ si)
                                       TFG.Flt -> return (mul er' ei' , sr ++ si)
                                       TFG.Cmx -> return (mul er' ei' , sr ++ si)
                                       _       -> fail "Type Error in Mul"
      FMWS.Add er ei           -> do (er' , sr) <- cmpImp er
                                     (ei' , si) <- cmpImp ei
                                     case t of
                                       TFG.Wrd -> return (add er' ei' , sr ++ si)
                                       TFG.Flt -> return (add er' ei' , sr ++ si)
                                       TFG.Cmx -> return (add er' ei' , sr ++ si)
                                       _       -> fail "Type Error in Add"
      FMWS.Sub er ei           -> do (er' , sr) <- cmpImp er
                                     (ei' , si) <- cmpImp ei
                                     case t of
                                       TFG.Wrd -> return (sub er' ei' , sr ++ si)
                                       TFG.Flt -> return (sub er' ei' , sr ++ si)
                                       TFG.Cmx -> return (sub er' ei' , sr ++ si)
                                       _       -> fail "Type Error in Sub"
      FMWS.Eql er ei           -> do (er' , sr) <- cmpImp er
                                     (ei' , si) <- cmpImp ei
                                     case sinTyp er of
                                       TFG.Wrd -> return (eql er' ei' , sr ++ si)
                                       TFG.Flt -> return (eql er' ei' , sr ++ si)
                                       TFG.Bol -> return (eql er' ei' , sr ++ si)
                                       _       -> fail "Type Error in Eql"

      FMWS.Ltd er ei           -> do (er' , sr) <- cmpImp er
                                     (ei' , si) <- cmpImp ei
                                     case sinTyp er of
                                       TFG.Wrd -> return (ltd er' ei' , sr ++ si)
                                       TFG.Flt -> return (ltd er' ei' , sr ++ si)
                                       TFG.Bol -> return (ltd er' ei' , sr ++ si)
                                       _       -> fail "Type Error in Ltd"


instance (n ~ Len r , HasSin TFG.Typ t , Compilable (b , ES.Env n String)) =>
         Compilable (FMWS.Exp r t -> b , ES.Env n String) where
 compile (ef , r) = let ?r = r in
   do v  <- newName
      t' <- cnvImpLft (sin :: TFG.Typ t)
      addParam (v , t')
      cmpImp (ef (FMWS.Tmp v))

cnvETEnv :: ( n ~ Len rr , r ~ TFG.Arg t, HasSin TFG.Typ t
           , ?r :: ES.Env n String) =>
           TFG.Typ t -> ET.Env (FMWS.Exp rr) r ->
           CompileMonad ([Exp] , [[Stmt]])
cnvETEnv t@(TFG.Arr _  tb) (ET.Ext e ess) = case TFG.getPrfHasSinArr t of
  (PrfHasSin , PrfHasSin) -> do (ee , se) <- cmpImp e
                                (es , ss) <- cnvETEnv tb ess
                                return (ee : es , se : ss)
cnvETEnv _                 ET.Emp         = return ([],[])
cnvETEnv _                 _              = impossibleM

class TypeCollectable a where
  collectTypes :: a -> [TFA.Typ]

instance HasSin TFG.Typ a => TypeCollectable (FMWS.Exp g a) where
  collectTypes ee  = let t  = sin :: TFG.Typ a in
                     (frmRgt (runNamM (cnv (t , ())))) : (case ee of
  -- type of primitive does not matter, but type of its darguemtns does
   FMWS.Prm v es -> TFG.fld (\ ls e -> ls ++ collectTypes e) [] (sinTyp v) es
   _             -> $(recAppMQ 'ee ''FMWS.Exp (const [| [] |]) ['FMWS.Prm]
     [| \ _x -> [] |] [| (++) |] [| (++) |] (trvWrp 't)
    (\ tt -> if
     | matchQ tt [t| FMWS.Exp a a -> FMWS.Exp a a |] -> [| collectTypes |]
     | matchQ tt [t| FMWS.Exp a a |]                 -> [| collectTypes |]
     | otherwise                                     -> [| const []     |])))

instance (HasSin TFG.Typ a , TypeCollectable b) =>
         TypeCollectable (FMWS.Exp g a -> b) where
  collectTypes f = let t  = sin :: TFG.Typ a in
                   (frmRgt (runNamM (cnv (t , ())))) :
                   collectTypes (f (FMWS.Tmp "_dummy"))

genStructs :: [TFA.Typ] -> String
genStructs ts = concat (
                ["typedef struct {Wrd size; "++s++"* elems;} Ary"++s++";\n"
                | TFA.Ary a <- ts,
                  a /= TFA.Wrd,
                  let s = show (pretty a)] ++
                ["typedef struct {"++sa++" fst; "++sb++" snd;} Tpl"++sa++sb++";\n"
                | TFA.Tpl a b <- ts,
                let sa = show (pretty a),
                let sb = show (pretty b)])

scompileWith :: (TypeCollectable a , Compilable (a , ES.Env n String)) =>
                [Var] -> TFG.Typ t -> ES.Env n String -> Word32 -> a -> ErrM String
scompileWith vs t r i e = let ?r = r in
                        do t' :: TFA.Typ <- runNamM (cnvImp t)
                           c <- runCompileMonad t'
                                (do mapM_ addParam vs
                                    cmpImp e) i
                           return ("#include \"header.h\"\n\n" ++
                                   genStructs (nub (collectTypes e)) ++ "\n" ++
                                   ((show . pretty) c))

scompile :: (TypeCollectable a , Compilable (a , ES.Env n String)) =>
                TFG.Typ t -> ES.Env n String -> a -> ErrM String
scompile t r e = scompileWith [] t r 0 e

icompileWith :: (TypeCollectable a , Compilable (a , ES.Env n String)) =>
                [Var] -> TFG.Typ t -> ES.Env n String -> Word32 -> a -> IO ()
icompileWith vs t r i e = (putStrLn . frmRgt . scompileWith vs t r i) e
