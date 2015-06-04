module QFeldspar.Inference where

import QFeldspar.MyPrelude

import QFeldspar.Nat.ADT
import qualified QFeldspar.Type.Herbrand as TH
import QFeldspar.Type.Herbrand hiding (Tpl,May,Cmx,Ary,App)
import QFeldspar.Solver
import QFeldspar.Conversion
import QFeldspar.Nat.Conversion ()
import QFeldspar.Expression.GADTTyped
import QFeldspar.Environment.Scoped  as ES
import QFeldspar.InferenceMonad

type TypFld = Typ (EnvFld '[])

mxm :: [Nat] -> Nat
mxm [] = Zro
mxm l  = maximum l

ind :: Exp m n (Maybe TypFld) -> Exp m n TypFld
ind e = (flip evalState
         (succ (maxMta (fmap (maybe (Mta Zro) id) e))) .
         mapM (maybe (do i <- getState
                         put (Suc i)
                         return (Mta i)) return)) e

typInf :: Exp m n (Maybe TypFld) -> (ES.Env m TypFld , ES.Env n TypFld)->
          NamM ErrM (Exp m n TypFld)
typInf e r = inf (ind e) r

maxMta :: Exp m n TypFld -> Nat
maxMta = mxm . concat . fmap mtas . toList

inf :: Exp m n TypFld -> (ES.Env m TypFld , ES.Env n TypFld) -> NamM ErrM (Exp m n TypFld)
inf e r = do let mts = (mxm . concat) [mtas x | x <- toList e]
                 (i' , cs) = execState (collect e r) (succ mts , [])
             mTs <- lift (slv (fmap Mta [Zro .. pred i']) cs)
             let ttas = zip [Zro ..] mTs

             return (fmap (appTtas ttas) e)

chk :: Exp m n TypFld -> (ES.Env m TypFld , ES.Env n TypFld) -> NamM ErrM TypFld
chk e r = do let mts = (mxm . concat) [mtas x | x <- toList e]
                 (t , (i' , cs)) = runState (collect e r) (mts , [])
             mTs <- lift (slv (fmap Mta [Zro .. pred i']) cs)
             let ttas = zip [Zro ..] mTs
             if length [() | Mta _ <- mTs] == 0
               then return (appTtas ttas t)
               else fail "Type Error!"

typChk :: forall m n t. (Cnv (TypFld, ()) t , Cnv (t, ()) TypFld) =>
          Exp m n t -> (ES.Env m t , ES.Env n t) -> NamM ErrM t
typChk e (s , g) = do s' :: ES.Env m TypFld <- traverse (flip (curry cnv) ()) s
                      g' :: ES.Env n TypFld <- traverse (flip (curry cnv) ()) g
                      e' :: Exp m n TypFld <- traverse (flip (curry cnv) ()) e
                      t <- chk e' (s' , g')
                      cnv (t , ())

matchArgs :: TypFld -> [TypFld] -> InfM (EnvFld '[]) TypFld
matchArgs tf []       = return tf
matchArgs tf (ta : ts) = do tb <- newMT
                            addC (tf :~: Arr ta tb)
                            matchArgs tb ts

collect :: Exp m n TypFld -> (ES.Env m TypFld , ES.Env n TypFld) ->
           InfM (EnvFld '[]) TypFld
collect ee (s , g) = case ee of
    ConI _         -> return TH.Wrd
    ConB _         -> return Bol
    ConF _         -> return Flt
    Var x          -> return (get x g)
    Prm  t x  es   -> do let tx = get x s
                         ts  <- mapM (flip collect (s , g)) es
                         addC (t :~: tx)
                         matchArgs tx ts
    Abs eb         -> do ta <- newMT
                         tb <- collect eb (s , Ext ta g)
                         return (Arr ta tb)
    App t  ef ea   -> do ta <- collect ea (s , g)
                         tf <- collect ef (s , g)
                         tb <- newMT
                         addC (tf :~: Arr ta tb)
                         addC (t  :~: ta)
                         return tb
    Cnd ec et ef   -> do tc <- collect ec (s , g)
                         addC (tc :~: Bol)
                         tt <- collect et (s , g)
                         tf <- collect ef (s , g)
                         addC (tt :~: tf)
                         return tt
    Whl ec eb ei   -> do tc <- collect ec (s , g)
                         tb <- collect eb (s , g)
                         ti <- collect ei (s , g)
                         ts <- newMT
                         addC (tc :~: Arr ts Bol)
                         addC (tb :~: Arr ts ts)
                         addC (ti :~: ts)
                         return ts
    Tpl ef es      -> TH.Tpl <$> collect ef (s , g) <*> collect es (s , g)
    Fst t e        -> do te  <- collect e (s , g)
                         tf  <- newMT
                         ts  <- newMT
                         addC (te :~: TH.Tpl tf ts)
                         addC (t  :~: ts)
                         return tf
    Snd t  e       -> do te  <- collect e (s , g)
                         tf  <- newMT
                         ts  <- newMT
                         addC (te :~: TH.Tpl tf ts)
                         addC (t  :~: tf)
                         return ts
    Ary el ef      -> do tl  <- collect el (s , g)
                         tf  <- collect ef (s , g)
                         addC (tl :~: TH.Wrd)
                         ta  <- newMT
                         addC (tf :~: Arr TH.Wrd ta)
                         return (TH.Ary ta)
    Len t e        -> do te  <- collect e (s , g)
                         ta  <- newMT
                         addC (te :~: TH.Ary ta)
                         addC (t  :~: ta)
                         return TH.Wrd
    Ind ea ei      -> do ta  <- collect ea (s , g)
                         taa <- newMT
                         addC (ta :~: TH.Ary taa)
                         ti  <- collect ei (s , g)
                         addC (ti :~: TH.Wrd)
                         return taa
    AryV el ef     -> do tl  <- collect el (s , g)
                         tf  <- collect ef (s , g)
                         addC (tl :~: TH.Wrd)
                         ta  <- newMT
                         addC (tf :~: Arr TH.Wrd ta)
                         return (TH.Vec ta)
    LenV t e       -> do te  <- collect e (s , g)
                         ta  <- newMT
                         addC (te :~: TH.Vec ta)
                         addC (t  :~: ta)
                         return TH.Wrd
    IndV ea ei     -> do ta  <- collect ea (s , g)
                         taa <- newMT
                         addC (ta :~: TH.Vec taa)
                         ti  <- collect ei (s , g)
                         addC (ti :~: TH.Wrd)
                         return taa
    LeT t  el eb   -> do tl  <- collect el (s , g)
                         tb  <- collect eb (s , Ext tl g)
                         addC (t :~: tl)
                         return tb
    Cmx er ei      -> do tr  <- collect er (s , g)
                         addC (tr :~: Flt)
                         ti  <- collect ei (s , g)
                         addC (ti :~: Flt)
                         return TH.Cmx
    Non            -> do t <- newMT
                         return (TH.May t)
    Som e          -> do t   <- collect e (s , g)
                         return (TH.May t)
    May t em en es -> do tm  <- collect em (s , g)
                         tn  <- collect en (s , g)
                         ts  <- collect es (s , g)
                         ta  <- newMT
                         addC (tm :~: TH.May ta)
                         addC (ts :~: Arr ta tn)
                         addC (t  :~: ta)
                         return tn
    Typ t e        -> do te <- collect e (s , g)
                         addC (t :~: te)
                         return te
    Mul el er      -> do tl <- collect el (s , g)
                         tr <- collect er (s , g)
                         addC (tl :~: tr)
                         return tl
    Add el er      -> do tl <- collect el (s , g)
                         tr <- collect er (s , g)
                         addC (tl :~: tr)
                         return tl
    Sub el er      -> do tl <- collect el (s , g)
                         tr <- collect er (s , g)
                         addC (tl :~: tr)
                         return tl
    Eql t el er    -> do tl <- collect el (s , g)
                         tr <- collect er (s , g)
                         addC (tl :~: tr)
                         addC (t  :~: tr)
                         return Bol
    Ltd t el er    -> do tl <- collect el (s , g)
                         tr <- collect er (s , g)
                         addC (tl :~: tr)
                         addC (t  :~: tr)
                         return Bol
    Int _          -> newMT
    Mem e          -> collect e (s , g)
    Fix e          -> do te <- collect e (s , g)
                         ta <- newMT
                         addC (te :~: Arr ta ta)
                         return ta
