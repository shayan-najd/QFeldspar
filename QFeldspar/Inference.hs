module QFeldspar.Inference where

import QFeldspar.MyPrelude

import QFeldspar.Nat.ADT
import qualified QFeldspar.Type.Herbrand as TH
import QFeldspar.Type.Herbrand hiding (Tpl,May,Cmx,Ary,App)
import QFeldspar.Solver
import QFeldspar.Conversion
import QFeldspar.Nat.Conversion ()
import QFeldspar.Expression.Feldspar.GADTTyped
import QFeldspar.Environment.Scoped  as ES
import QFeldspar.InferenceMonad

type TypFld = Typ (EnvFld '[])

mxm :: [Nat] -> Nat
mxm [] = Zro
mxm l  = maximum l

ind :: Exp n (Maybe TypFld) -> Exp n TypFld
ind e = (flip evalState
         (succ (maxMta (fmap (maybe (Mta Zro) id) e))) .
         mapM (maybe (do i <- getState
                         put (Suc i)
                         return (Mta i)) return)) e

typInf :: Exp n (Maybe TypFld) -> ES.Env n TypFld ->
          ErrM (Exp n TypFld)
typInf e r = inf (ind e) r

maxMta :: Exp n TypFld -> Nat
maxMta = mxm . concat . fmap mtas . toList

inf :: Exp n TypFld -> ES.Env n TypFld -> ErrM (Exp n TypFld)
inf e r = do let mts = (mxm . concat) [mtas x | x <- toList e]
                 (i' , cs) = execState (collect e r) (succ mts , [])
             mTs <- slv (fmap Mta [Zro .. pred i']) cs
             let ttas = zip [Zro ..] mTs

             return (fmap (appTtas ttas) e)

chk :: Exp n TypFld -> ES.Env n TypFld -> ErrM TypFld
chk e r = do let mts = (mxm . concat) [mtas x | x <- toList e]
                 (t , (i' , cs)) = runState (collect e r) (mts , [])
             mTs <- slv (fmap Mta [Zro .. pred i']) cs
             let ttas = zip [Zro ..] mTs
             if length [() | Mta _ <- mTs] == 0
               then return (appTtas ttas t)
               else fail "Type Error!"

typChk :: forall n t. (Cnv (TypFld, ()) t , Cnv (t, ()) TypFld) =>
          Exp n t -> ES.Env n t-> ErrM t
typChk e r = do r' :: ES.Env n TypFld <- traverse
                      (flip (curry cnv) ()) r
                e' :: Exp n TypFld <- traverse (flip (curry cnv) ()) e
                t <- chk e' r'
                cnv (t , ())

collect :: Exp n TypFld -> ES.Env n TypFld ->
         InfM (EnvFld '[]) TypFld
collect ee r = case ee of
    ConI _         -> return Int
    ConB _         -> return Bol
    ConF _         -> return Flt
    Var x          -> return (get x r)
    Abs eb         -> do ta <- newMT
                         tb <- collect eb (Ext ta r)
                         return (Arr ta tb)
    App t  ef ea   -> do ta <- collect ea r
                         tf <- collect ef r
                         tb <- newMT
                         addC (tf :~: Arr ta tb)
                         addC (t  :~: ta)
                         return tb
    Cnd ec et ef   -> do tc <- collect ec r
                         addC (tc :~: Bol)
                         tt <- collect et r
                         tf <- collect ef r
                         addC (tt :~: tf)
                         return tt
    Whl ec eb ei   -> do t  <- newMT
                         tc <- collect ec (Ext t r)
                         addC (tc :~: Bol)
                         tb <- collect eb (Ext t r)
                         addC (tb :~: t)
                         ti <- collect ei r
                         addC (ti :~: t)
                         return t
    Tpl ef es      -> TH.Tpl <$> collect ef r <*> collect es r
    Fst t e        -> do te  <- collect e r
                         tf  <- newMT
                         ts  <- newMT
                         addC (te :~: TH.Tpl tf ts)
                         addC (t  :~: ts)
                         return tf
    Snd t  e       -> do te  <- collect e r
                         tf  <- newMT
                         ts  <- newMT
                         addC (te :~: TH.Tpl tf ts)
                         addC (t  :~: tf)
                         return ts
    Ary el ef      -> do tl  <- collect el r
                         addC (tl :~: Int)
                         tf  <- collect ef (Ext Int r)
                         return (TH.Ary tf)
    Len t e        -> do te  <- collect e r
                         ta  <- newMT
                         addC (te :~: TH.Ary ta)
                         addC (t  :~: ta)
                         return Int
    Ind ea ei      -> do ta  <- collect ea r
                         taa <- newMT
                         addC (ta :~: TH.Ary taa)
                         ti  <- collect ei r
                         addC (ti :~: Int)
                         return taa
    AryV el ef     -> do tl  <- collect el r
                         addC (tl :~: Int)
                         tf  <- collect ef (Ext Int r)
                         return (TH.Vec tf)
    LenV t e       -> do te  <- collect e r
                         ta  <- newMT
                         addC (te :~: TH.Vec ta)
                         addC (t  :~: ta)
                         return Int
    IndV ea ei     -> do ta  <- collect ea r
                         taa <- newMT
                         addC (ta :~: TH.Vec taa)
                         ti  <- collect ei r
                         addC (ti :~: Int)
                         return taa
    Let t  el eb   -> do tl  <- collect el r
                         tb  <- collect eb (Ext tl r)
                         addC (t :~: tl)
                         return tb
    Cmx er ei      -> do tr  <- collect er r
                         addC (tr :~: Flt)
                         ti  <- collect ei r
                         addC (ti :~: Flt)
                         return TH.Cmx
    Non            -> do t <- newMT
                         return (TH.May t)
    Som e          -> do t   <- collect e r
                         return (TH.May t)
    May t em en es -> do mm  <- collect em r
                         a   <- newMT
                         addC (mm :~: TH.May a)
                         bn  <- collect en r
                         bs  <- collect es (Ext a r)
                         addC (bn :~: bs)
                         addC (t  :~: a)
                         return bs
    Typ t e        -> do te <- collect e r
                         addC (t :~: te)
                         return te
    Mul el er      -> do tl <- collect el r
                         tr <- collect er r
                         addC (tl :~: tr)
                         return tl
