module QFeldspar.Compiler where

import QFeldspar.MyPrelude hiding (fst,snd)

import QFeldspar.Expression.C
import qualified QFeldspar.Expression.MiniFeldspar as MFS

import qualified QFeldspar.Type.ADT as TA
import qualified QFeldspar.Type.GADT as TG

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

runCompileMonad :: TA.Typ -> CompileMonad (Exp , [Stmt]) -> Word32 ->
                   ErrM Func
runCompileMonad ty m i = do ((exp,stmts),(_,ps,vs)) <- runStateT m (i,[],[])
                            pure (Func ty "func" ps
                                  ([Declare (v , t) | (v , t) <- vs] ++
                                   stmts ++ [Return exp]))

cnvWthLft :: (Cnv (a, r) b) =>
             r -> a -> CompileMonad b
cnvWthLft r = lift . runNamM . cnvWth r

cmpWth :: (Compilable (t, r)) =>
          r -> t -> CompileMonad (Exp , [Stmt])
cmpWth r e = compile (e , r)

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

newTpl :: TA.Typ -> Exp -> Exp -> Exp
newTpl (TA.Tpl tf ts) ef es = App "newTpl"
                               [Var (show (pretty tf) ++ show (pretty ts)) , ef , es]
newTpl _ _ _  = impossible

newAry :: TA.Typ -> Exp -> Exp
newAry (TA.Ary t) e  = App "newAry" [Var (show (pretty t)) , e]
newAry _           _  = impossible

class Compilable t where
 compile :: t -> CompileMonad (Exp , [Stmt])

instance (TG.Type t, n ~ Len r) =>
         Compilable (MFS.Exp r t, ES.Env n String) where
  compile (ee , r) = do
    let t = sin :: TG.Typ t
    t'  <- cnvWthLft r t
    case ee of
      MFS.Tag _ e       -> cmpWth r e
      MFS.Mem e         -> cmpWth r e
      MFS.Tmp  x        -> return (Var x , [])
      MFS.ConI i        -> return (Wrd i , [])
      MFS.ConB True     -> return (tru   , [])
      MFS.ConB False    -> return (fls   , [])
      MFS.ConF f        -> return (Flt f , [])
      MFS.Prm  v ET.Emp -> do v' :: VS.Var n <- cnvWthLft r v
                              let ve = ES.get v' r
                              return (Var ve , [])
      MFS.Prm  v es     -> do v' :: VS.Var n <- cnvWthLft r v
                              let ve         = ES.get v' r
                              (es' , ss)     <- cnvETEnv r es
                              return (App ve es' ,concat ss)
      MFS.Cnd ec et ef  -> do (ec' , sc) <- cmpWth r ec
                              (et' , st) <- cmpWth r et
                              (ef' , sf) <- cmpWth r ef
                              v <- newName
                              addVar (v , t')
                              return (Var v
                                     , sc ++
                                      [If ec'
                                       (st ++ [Assign v et'])
                                       (sf ++ [Assign v ef'])])
      MFS.Whl ec eb ei -> do xs <- newName
                             addVar (xs , t')
                             (ec' , sc) <- cmpWth r (ec (MFS.Tmp xs))
                             (eb' , sb) <- cmpWth r (eb (MFS.Tmp xs))
                             (ei' , si) <- cmpWth r ei
                             return ( Var xs
                                    , si ++
                                      [Assign xs ei'] ++
                                      sc ++
                                      [Whl ec'
                                       (sb ++
                                       [Assign xs eb'] ++ sc)])
      MFS.Tpl ef es           -> case TG.getPrfHasSinTpl t of
       (PrfHasSin , PrfHasSin) -> do (ef' , sf) <- cmpWth r ef
                                     (es' , ss) <- cmpWth r es
                                     return (newTpl t' ef' es', sf ++ ss)
      MFS.Fst e               -> do (e'  , se) <- cmpWth r e
                                    return (fst e' , se)
      MFS.Snd e               -> do (e' , se) <- cmpWth r e
                                    return (snd e' , se)
      MFS.Ary l f             -> case TG.getPrfHasSinAry t of
        PrfHasSin              -> do xl <- newName
                                     addVar (xl , TA.Wrd)
                                     xa <- newName
                                     addVar (xa , t')
                                     xi <- newName
                                     addVar (xi , TA.Wrd)
                                     (el , sl) <- cmpWth r l
                                     (ef , sf) <- cmpWth r (f (MFS.Tmp xi))
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
      MFS.Len e               -> do (e'  , se) <- cmpWth r e
                                    return (len e' , se)
      MFS.Ind ea ei           -> do (ea' , sa) <- cmpWth r ea
                                    (ei' , si) <- cmpWth r ei
                                    return (ind ea' ei' , sa ++ si)
      MFS.LeT el eb           -> do xl <- newName
                                    tl <- cnvWthLft r (sinTypOf el t)
                                    addVar (xl , tl)
                                    (el' , sl) <- cmpWth r el
                                    (eb' , sb) <- cmpWth r (eb (MFS.Tmp xl))
                                    return (eb' , sl ++
                                                  [Assign xl el'] ++
                                                  sb)
      MFS.Cmx er ei           -> do (er' , sr) <- cmpWth r er
                                    (ei' , si) <- cmpWth r ei
                                    return (cmx er' ei' , sr ++ si)
      MFS.Mul er ei           -> do (er' , sr) <- cmpWth r er
                                    (ei' , si) <- cmpWth r ei
                                    case t of
                                       TG.Wrd -> return (mul er' ei' , sr ++ si)
                                       TG.Flt -> return (mul er' ei' , sr ++ si)
                                       TG.Cmx -> return (mul er' ei' , sr ++ si)
                                       _       -> fail "Type Error in Mul"
      MFS.Add er ei           -> do (er' , sr) <- cmpWth r er
                                    (ei' , si) <- cmpWth r ei
                                    case t of
                                       TG.Wrd -> return (add er' ei' , sr ++ si)
                                       TG.Flt -> return (add er' ei' , sr ++ si)
                                       TG.Cmx -> return (add er' ei' , sr ++ si)
                                       _       -> fail "Type Error in Add"
      MFS.Sub er ei           -> do (er' , sr) <- cmpWth r er
                                    (ei' , si) <- cmpWth r ei
                                    case t of
                                       TG.Wrd -> return (sub er' ei' , sr ++ si)
                                       TG.Flt -> return (sub er' ei' , sr ++ si)
                                       TG.Cmx -> return (sub er' ei' , sr ++ si)
                                       _       -> fail "Type Error in Sub"
      MFS.Eql er ei           -> do (er' , sr) <- cmpWth r er
                                    (ei' , si) <- cmpWth r ei
                                    case sinTyp er of
                                       TG.Wrd -> return (eql er' ei' , sr ++ si)
                                       TG.Flt -> return (eql er' ei' , sr ++ si)
                                       TG.Bol -> return (eql er' ei' , sr ++ si)
                                       _       -> fail "Type Error in Eql"

      MFS.Ltd er ei           -> do (er' , sr) <- cmpWth r er
                                    (ei' , si) <- cmpWth r ei
                                    case sinTyp er of
                                       TG.Wrd -> return (ltd er' ei' , sr ++ si)
                                       TG.Flt -> return (ltd er' ei' , sr ++ si)
                                       TG.Bol -> return (ltd er' ei' , sr ++ si)
                                       _       -> fail "Type Error in Ltd"


instance (n ~ Len r , TG.Type t , Compilable (b , ES.Env n String)) =>
         Compilable (MFS.Exp r t -> b , ES.Env n String) where
 compile (ef , r) =
   do v  <- newName
      t' <- cnvWthLft r (sin :: TG.Typ t)
      addParam (v , t')
      cmpWth r (ef (MFS.Tmp v))

-- cnvWth r
cnvETEnv :: (n ~ Len rr , TG.Types d) =>
           ES.Env n String -> ET.Env (MFS.Exp rr) d ->
           CompileMonad ([Exp] , [[Stmt]])
cnvETEnv r d@(ET.Ext e ess) = case TG.getPrfHasSinEnvOf d of
  (PrfHasSin , PrfHasSin)  -> do (ee , se) <- cmpWth r e
                                 (es , ss) <- cnvETEnv r ess
                                 return (ee : es , se : ss)
cnvETEnv _ ET.Emp           = return ([],[])

class TypeCollectable a where
  collectTypes :: a -> [TA.Typ]

instance TG.Type a => TypeCollectable (MFS.Exp g a) where
  collectTypes ee  = let t  = sin :: TG.Typ a in
                     (frmRgt (runNamM (cnv (t , ())))) : (case ee of
  -- type of primitive does not matter, but type of its darguemtns does
   MFS.Prm _ es -> TG.fld (\ ls e -> ls ++ collectTypes e) [] es
   _             -> $(recAppMQ 'ee ''MFS.Exp (const [| [] |]) ['MFS.Prm]
     [| \ _x -> [] |] [| (++) |] [| (++) |] (trvWrp 't)
    (\ tt -> if
     | matchQ tt [t| MFS.Exp a a -> MFS.Exp a a |] -> [| collectTypes |]
     | matchQ tt [t| MFS.Exp a a |]                 -> [| collectTypes |]
     | otherwise                                     -> [| const []     |])))

instance (TG.Type a , TypeCollectable b) =>
         TypeCollectable (MFS.Exp g a -> b) where
  collectTypes f = let t  = sin :: TG.Typ a in
                   (frmRgt (runNamM (cnv (t , ())))) :
                   collectTypes (f (MFS.Tmp "_dummy"))

genStructs :: [TA.Typ] -> String
genStructs ts = concat (
                ["typedef struct {Wrd size; "++s++"* elems;} Ary"++s++";\n"
                | TA.Ary a <- ts,
                  a /= TA.Wrd,
                  let s = show (pretty a)] ++
                ["typedef struct {"++sa++" fst; "++sb++" snd;} Tpl"++sa++sb++";\n"
                | TA.Tpl a b <- ts,
                let sa = show (pretty a),
                let sb = show (pretty b)])

scompileWith :: (TypeCollectable a , Compilable (a , ES.Env n String)) =>
                [Var] -> TG.Typ t -> ES.Env n String -> Word32 -> a -> ErrM String
scompileWith vs t r i e =
                        do t' :: TA.Typ <- runNamM (cnvWth r t)
                           c <- runCompileMonad t'
                                (do mapM_ addParam vs
                                    cmpWth r e) i
                           return ("#include \"header.h\"\n\n" ++
                                   genStructs (nub (collectTypes e)) ++ "\n" ++
                                   ((show . pretty) c))

scompile :: (TypeCollectable a , Compilable (a , ES.Env n String)) =>
                TG.Typ t -> ES.Env n String -> a -> ErrM String
scompile t r e = scompileWith [] t r 0 e

icompileWith :: (TypeCollectable a , Compilable (a , ES.Env n String)) =>
                [Var] -> TG.Typ t -> ES.Env n String -> Word32 -> a -> IO ()
icompileWith vs t r i e = (putStrLn . frmRgt . scompileWith vs t r i) e
