module QFeldspar.Expression.Feldspar.MiniFeldspar
    (Exp(..),sucAll,prdAll,mapVar,isFresh,absVar,absTmp,eql,eqlF
    ,cntTmp,hasOneOrZro,pattern TF) where

import QFeldspar.MyPrelude hiding (foldl)
import GHC.Show
import qualified QFeldspar.Type.Feldspar.GADT     as TFG

import QFeldspar.Variable.Typed

import QFeldspar.Environment.Typed as ET
import QFeldspar.Singleton

data Exp :: [*] -> * -> * where
  ConI  :: Int      -> Exp r Int
  ConB  :: Bool     -> Exp r Bol
  ConF  :: Float    -> Exp r Flt
  AppV  :: HasSin TFG.Typ t =>
           Var r t  -> Env (Exp r) (TFG.Arg t) -> Exp r (TFG.Out t)
  Cnd   :: Exp r Bol -> Exp r t -> Exp r t -> Exp r t
  Whl   :: (Exp r t -> Exp r Bol) -> (Exp r t -> Exp r t) ->
           Exp r t  -> Exp r t
  Tpl   :: Exp r tf -> Exp r ts -> Exp r (Tpl tf ts)
  Fst   :: HasSin TFG.Typ ts =>
           Exp r (Tpl tf ts)-> Exp r tf
  Snd   :: HasSin TFG.Typ tf =>
           Exp r (Tpl tf ts)-> Exp r ts
  Ary   :: Exp r Int -> (Exp r Int -> Exp r t) -> Exp r (Ary t)
  Len   :: HasSin TFG.Typ ta =>
           Exp r (Ary ta) -> Exp r Int
  Ind   :: Exp r (Ary ta) -> Exp r Int -> Exp r ta
  Let   :: HasSin TFG.Typ tl =>
           Exp r tl -> (Exp r tl -> Exp r tb) -> Exp r tb
  Cmx   :: Exp r Flt -> Exp r Flt -> Exp r Cmx
  Tmp   :: String -> Exp r t  -- dummy constructor
  Tag   :: String -> Exp r t -> Exp r t
  Mul   :: Exp r t -> Exp r t -> Exp r t

instance Show (Exp r ta -> Exp r tb) where
  show f = let v = genNewNam "x"
               {-# NOINLINE v #-}
           in deepseq v $ ("(\\ "++ v ++ " -> (" ++
                     show (f (Tmp v))
                              ++ "))")

deriving instance Show (Env (Exp r) r')

tagFree :: Exp r t -> Exp r t
tagFree (Tag _ e) = tagFree e
tagFree e         = e

pattern TF e <- (tagFree -> e)

{-
tag :: Maybe String -> Exp r t -> Exp r t
tag = maybe id Tag

-- Assumption: one tag per node in the AST
unTag :: Exp r t -> (Exp r t , Maybe String)
unTag (Tag x e) = (e , Just x)
unTag e         = (e , Nothing)
-}

eqlE :: Env (Exp r) r' -> Env (Exp r) r' -> Bool
eqlE Emp        Emp        = True
eqlE (Ext x xs) (Ext y ys) = eql x y && eqlE xs ys
eqlE _          _          = False

eql :: forall r t .  Exp r t -> Exp r t -> Bool
eql (ConI i)    (ConI i')     = i == i'
eql (ConB b)    (ConB b')     = b == b'
eql (ConF f)    (ConF f')     = f == f'
eql (AppV (v :: Var r tv) es)    (AppV (v' :: Var r tv') es') =
  case eqlSin (sin :: TFG.Typ tv) (sin :: TFG.Typ tv') of
    Rgt Rfl -> v == v' && eqlE es es'
    _       -> False
eql (Cnd ec et ef) (Cnd ec' et' ef') = eql ec ec' && eql et et' && eql ef ef'
eql (Whl ec eb ei) (Whl ec' eb' ei') = eqlF ec ec' && eqlF eb eb' && eql ei ei'
eql (Tpl ef es)    (Tpl ef' es')     = eql ef ef' && eql es es'
eql (Fst (e :: Exp r (Tpl t ts))) (Fst (e' :: Exp r (Tpl t ts'))) =
  case eqlSin (sin :: TFG.Typ ts) (sin :: TFG.Typ ts') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Snd (e :: Exp r (Tpl tf t))) (Snd (e' :: Exp r (Tpl tf' t))) =
  case eqlSin (sin :: TFG.Typ tf) (sin :: TFG.Typ tf') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Ary ei ef) (Ary ei' ef') = eql ei ei' && eqlF ef ef'
eql (Len (e :: Exp r (Ary ta))) (Len (e' :: Exp r (Ary ta'))) =
  case eqlSin (sin :: TFG.Typ ta) (sin :: TFG.Typ ta') of
    Rgt Rfl -> eql e e'
    _       -> False
eql (Ind (e :: Exp r (Ary t)) ei) (Ind (e' :: Exp r (Ary t)) ei') =
    eql e e' && eql ei ei'
eql (Let (el :: Exp r ta) eb) (Let (el' :: Exp r ta') eb') =
  case eqlSin (sin :: TFG.Typ ta) (sin :: TFG.Typ ta') of
    Rgt Rfl -> eql el el' && eqlF eb eb'
    _       -> False
eql (Cmx ei er) (Cmx ei' er') = eql ei ei' && eql er er'
eql (Tmp x    ) (Tmp x')      = x == x'
eql (Tag _ e)   e'            = eql e e'
eql e          (Tag _ e')     = eql e e'
eql (Mul ei er) (Mul ei' er') = eql ei ei' && eql er er'
eql _           _             = False

eqlF :: forall r ta tb.  (Exp r ta -> Exp r tb) -> (Exp r ta -> Exp r tb) -> Bool
eqlF f f' = let v = genNewNam "__eqlFMS__"
                {-# NOINLINE v #-}
            in deepseq v $ eql (f (Tmp v)) (f' (Tmp v))

sucAll :: Exp r t' -> Exp (t ': r) t'
sucAll = mapVar Suc prd

prdAll :: Exp (t ': r) t' -> Exp r t'
prdAll = mapVar prd Suc

mapVar :: (forall t'. Var r  t' -> Var r' t') ->
          (forall t'. Var r' t' -> Var r  t') ->
          Exp r t -> Exp r' t
mapVar _ _ (ConI i)       = ConI i
mapVar _ _ (ConB i)       = ConB i
mapVar _ _ (ConF i)       = ConF i
mapVar f g (AppV v es)    = AppV (f v) (ET.fmap (mapVar f g) es)
mapVar f g (Cnd ec et ef) = Cnd (mapVar f g ec) (mapVar f g et) (mapVar f g ef)
mapVar f g (Whl ec eb ei) = Whl (mapVarF f g ec)
                                (mapVarF f g eb) (mapVar f g ei)
mapVar f g (Tpl ef es)    = Tpl (mapVar f g ef) (mapVar f g es)
mapVar f g (Fst e)        = Fst (mapVar f g e)
mapVar f g (Snd e)        = Snd (mapVar f g e)
mapVar f g (Ary el ef)    = Ary (mapVar f g el) (mapVarF f g ef)
mapVar f g (Len e)        = Len (mapVar f g e)
mapVar f g (Ind ea ei)    = Ind (mapVar f g ea) (mapVar f g ei)
mapVar f g (Let el eb)    = Let (mapVar f g el) (mapVarF f g eb)
mapVar f g (Cmx er ei)    = Cmx (mapVar f g er) (mapVar f g ei)
mapVar _ _ (Tmp x)        = Tmp x
mapVar f g (Tag x e)      = Tag x (mapVar f g e)
mapVar f g (Mul er ei)    = Mul (mapVar f g er) (mapVar f g ei)

mapVarF :: (forall t'. Var r  t' -> Var r' t') ->
           (forall t'. Var r' t' -> Var r  t') ->
           (Exp r a -> Exp r b) -> (Exp r' a -> Exp r' b)
mapVarF f g ff = mapVar f g . ff . mapVar g f

absTmp :: forall r t t'. (HasSin TFG.Typ t', HasSin TFG.Typ t) =>
          Exp r t' -> String -> Exp r t -> Exp r t
absTmp xx s ee = let t = sin :: TFG.Typ t in case ee of
  ConI i                    -> ConI i
  ConB i                    -> ConB i
  ConF i                    -> ConF i
  AppV v es                 -> AppV v (TFG.mapC (sinTyp v) (absTmp xx s) es)
  Cnd ec et ef              -> Cnd (absTmp xx s ec)   (absTmp xx s et)
                                    (absTmp xx s ef)
  Whl ec eb ei              -> Whl (absTmp xx s . ec) (absTmp xx s . eb)
                                   (absTmp xx s ei)
  Tpl ef es                 -> case TFG.getPrfHasSinTpl t of
    (PrfHasSin , PrfHasSin) -> Tpl (absTmp xx s ef)   (absTmp xx s es)
  Fst e                     -> Fst (absTmp xx s e)
  Snd e                     -> Snd (absTmp xx s e)
  Ary el ef                 -> case TFG.getPrfHasSinAry t of
    PrfHasSin               -> Ary (absTmp xx s el)   (absTmp xx s . ef)
  Len e                     -> Len (absTmp xx s e)
  Ind ea ei                 -> Ind (absTmp xx s ea)   (absTmp xx s ei)
  Let el eb                 -> Let (absTmp xx s el)   (absTmp xx s . eb)
  Cmx er ei                 -> Cmx (absTmp xx s er)   (absTmp xx s ei)
  Tmp x
    | s == x                -> case eqlSin (sinTyp xx) t of
      Rgt Rfl               -> xx
      _                     -> ee
    | otherwise             -> ee
  Tag x e                   -> Tag x (absTmp xx s e)
  Mul er ei                 -> Mul (absTmp xx s er) (absTmp xx s ei)

absVar :: forall r a b. (HasSin TFG.Typ a, HasSin TFG.Typ b) =>
         Exp (a ': r) b -> Exp r a -> Exp r b
absVar ee xx = absVar' xx ee

absVar' :: forall r a b. (HasSin TFG.Typ a, HasSin TFG.Typ b) =>
          Exp r a -> Exp (a ': r) b -> Exp r b
absVar' xx ee = let b = sin :: TFG.Typ b in case ee of
  ConI i                    -> ConI i
  ConB i                    -> ConB i
  ConF i                    -> ConF i
  AppV v@Zro _              -> case sinTyp v of
    TFG.Int                 -> xx
    TFG.Bol                 -> xx
    TFG.Flt                 -> xx
    TFG.Tpl _ _             -> xx
    TFG.Ary _               -> xx
    TFG.Vct _               -> xx
    TFG.May _               -> xx
    TFG.Cmx                 -> xx
    TFG.Arr _ _             -> impossible
  AppV v   es               -> AppV (prd v) (TFG.mapC (sinTyp v) (absVar' xx) es)
  Cnd ec et ef              -> Cnd (absVar' xx ec)   (absVar' xx et)
                                    (absVar' xx ef)
  Whl ec eb ei              -> Whl (absVar'F xx ec) (absVar'F xx eb)
                                   (absVar' xx ei)
  Tpl ef es                 -> case TFG.getPrfHasSinTpl b of
    (PrfHasSin , PrfHasSin) -> Tpl (absVar' xx ef)   (absVar' xx es)
  Fst e                     -> Fst (absVar' xx e)
  Snd e                     -> Snd (absVar' xx e)
  Ary el ef                 -> case TFG.getPrfHasSinAry b of
    PrfHasSin               -> Ary (absVar' xx el)   (absVar'F xx ef)
  Len e                     -> Len (absVar' xx e)
  Ind ea ei                 -> Ind (absVar' xx ea)   (absVar' xx ei)
  Let el eb                 -> Let (absVar' xx el)   (absVar'F xx eb)
  Cmx er ei                 -> Cmx (absVar' xx er)   (absVar' xx ei)
  Tmp x                     -> Tmp x
  Tag x e                   -> Tag x (absVar' xx e)
  Mul er ei                 -> Mul (absVar' xx er)   (absVar' xx ei)

absVar'F :: forall r a b c.
            (HasSin TFG.Typ a, HasSin TFG.Typ b, HasSin TFG.Typ c) =>
           Exp r a -> (Exp (a ': r) b -> Exp (a ': r) c) ->
                      (Exp r b -> Exp r c)
absVar'F xx ef = let v = genNewNam "absVar'F"
                     {-# NOINLINE v #-}
                 in deepseq v $ (\ x -> absTmp x v (absVar' xx (ef (Tmp v))))

-- when input string is not "__dummy__"
hasTmp :: String -> Exp r t -> Bool
hasTmp s ee = case ee of
  ConI _                    -> False
  ConB _                    -> False
  ConF _                    -> False
  AppV _ es                 -> foldl (\ b e -> b || hasTmp s e) False es
  Cnd ec et ef              -> hasTmp  s ec || hasTmp  s et || hasTmp  s ef
  Whl ec eb ei              -> hasTmpF s ec || hasTmpF s eb || hasTmp  s ei
  Tpl ef es                 -> hasTmp  s ef || hasTmp  s es
  Fst e                     -> hasTmp  s e
  Snd e                     -> hasTmp  s e
  Ary el ef                 -> hasTmp  s el || hasTmpF s ef
  Len e                     -> hasTmp  s e
  Ind ea ei                 -> hasTmp  s ea || hasTmp  s ei
  Let el eb                 -> hasTmp  s el || hasTmpF s eb
  Cmx er ei                 -> hasTmp  s er || hasTmp  s ei
  Tmp x
    | s == x                -> True
    | otherwise             -> False
  Tag _ e                   -> hasTmp  s e
  Mul er ei                 -> hasTmp  s er || hasTmp  s ei

hasTmpF :: String -> (Exp r ta -> Exp r tb) -> Bool
hasTmpF s f = let v = genNewNam "hasTmpF"
                  {-# NOINLINE v #-}
              in  deepseq v $ hasTmp s (f (Tmp v))

-- when input string is not "__dummy__"
cntTmp :: String -> Exp r t -> Int
cntTmp s ee = case ee of
  ConI _                    -> 0
  ConB _                    -> 0
  ConF _                    -> 0
  AppV _ es                 -> foldl (\ b e -> b + cntTmp s e) 0 es
  Cnd ec et ef              -> cntTmp  s ec + cntTmp  s et + cntTmp  s ef
  Whl ec eb ei              -> cntTmpF s ec + cntTmpF s eb + cntTmp  s ei
  Tpl ef es                 -> cntTmp  s ef + cntTmp  s es
  Fst e                     -> cntTmp  s e
  Snd e                     -> cntTmp  s e
  Ary el ef                 -> cntTmp  s el + cntTmpF s ef
  Len e                     -> cntTmp  s e
  Ind ea ei                 -> cntTmp  s ea + cntTmp  s ei
  Let el eb                 -> cntTmp  s el + cntTmpF s eb
  Cmx er ei                 -> cntTmp  s er + cntTmp  s ei
  Tmp x
    | s == x                -> 1
    | otherwise             -> 0
  Tag _ e                   -> cntTmp  s e
  Mul er ei                 -> cntTmp  s er + cntTmp  s ei

cntTmpF :: String -> (Exp r ta -> Exp r tb) -> Int
cntTmpF s f = let v = genNewNam "cntTmpF"
                  {-# NOINLINE v #-}
              in  deepseq v $ cntTmp s (f (Tmp v))

hasOneOrZro :: (Exp r ta -> Exp r tb) -> Bool
hasOneOrZro f = let v = genNewNam "hasOneOrZro"
                    {-# NOINLINE v #-}
                in  deepseq v $ cntTmp v (f (Tmp v)) <= 1

isFresh :: (Exp r ta -> Exp r tb) -> Bool
isFresh f = let v = genNewNam "isFresh"
                {-# NOINLINE v #-}
            in  deepseq v $ not (hasTmp v (f (Tmp v)))


instance Show
             (Exp
                r_aawf t_aawg) where
    showsPrec
      a_aaEz
      (ConI b1_aaEA)
      = showParen
          ((a_aaEz >= 11))
          ((.)
             (showString "ConI ") (showsPrec 11 b1_aaEA))
    showsPrec
      a_aaEB
      (ConB b1_aaEC)
      = showParen
          ((a_aaEB >= 11))
          ((.)
             (showString "ConB ") (showsPrec 11 b1_aaEC))
    showsPrec
      a_aaED
      (ConF b1_aaEE)
      = showParen
          ((a_aaED >= 11))
          ((.)
             (showString "ConF ") (showsPrec 11 b1_aaEE))
    showsPrec
      a_aaEF
      (AppV b1_aaEG b2_aaEH)
      = showParen
          ((a_aaEF >= 11))
          ((.)
             (showString "AppV ")
             ((.)
                (showsPrec 11 b1_aaEG)
                ((.) showSpace (showsPrec 11 b2_aaEH))))
    showsPrec
      a_aaEI
      (Cnd b1_aaEJ
                                                        b2_aaEK
                                                        b3_aaEL)
      = showParen
          ((a_aaEI >= 11))
          ((.)
             (showString "Cnd ")
             ((.)
                (showsPrec 11 b1_aaEJ)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_aaEK)
                      ((.)
                         showSpace (showsPrec 11 b3_aaEL))))))
    showsPrec
      a_aaEM
      (Whl b1_aaEN
                                                        b2_aaEO
                                                        b3_aaEP)
      = showParen
          ((a_aaEM >= 11))
          ((.)
             (showString "Whl ")
             ((.)
                (showsPrec 11 b1_aaEN)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_aaEO)
                      ((.)
                         showSpace (showsPrec 11 b3_aaEP))))))
    showsPrec
      a_aaEQ
      (Tpl b1_aaER b2_aaES)
      = showParen
          ((a_aaEQ >= 11))
          ((.)
             (showString "Tpl ")
             ((.)
                (showsPrec 11 b1_aaER)
                ((.) showSpace (showsPrec 11 b2_aaES))))
    showsPrec
      a_aaET
      (Fst b1_aaEU)
      = showParen
          ((a_aaET >= 11))
          ((.)
             (showString "Fst ") (showsPrec 11 b1_aaEU))
    showsPrec
      a_aaEV
      (Snd b1_aaEW)
      = showParen
          ((a_aaEV >= 11))
          ((.)
             (showString "Snd ") (showsPrec 11 b1_aaEW))
    showsPrec
      a_aaEX
      (Ary b1_aaEY b2_aaEZ)
      = showParen
          ((a_aaEX >= 11))
          ((.)
             (showString "Ary ")
             ((.)
                (showsPrec 11 b1_aaEY)
                ((.) showSpace (showsPrec 11 b2_aaEZ))))
    showsPrec
      a_aaF0
      (Len b1_aaF1)
      = showParen
          ((a_aaF0 >= 11))
          ((.)
             (showString "Len ") (showsPrec 11 b1_aaF1))
    showsPrec
      a_aaF2
      (Ind b1_aaF3 b2_aaF4)
      = showParen
          ((a_aaF2 >= 11))
          ((.)
             (showString "Ind ")
             ((.)
                (showsPrec 11 b1_aaF3)
                ((.) showSpace (showsPrec 11 b2_aaF4))))

    showsPrec
      a_aaF5
      (Let b1_aaF6 b2_aaF7)
      = showParen
          ((a_aaF5 >= 11))
          ((.)
             (showString "Let ")
             ((.)
                (showsPrec 11 b1_aaF6)
                ((.) showSpace (showsPrec 11 b2_aaF7))))
    showsPrec
      a_aaF8
      (Cmx b1_aaF9 b2_aaFa)
      = showParen
          ((a_aaF8 >= 11))
          ((.)
             (showString "Cmx ")
             ((.)
                (showsPrec 11 b1_aaF9)
                ((.) showSpace (showsPrec 11 b2_aaFa))))
    showsPrec
      a_aaFb
      (Tmp b1_aaFc)
      = showParen
          ((a_aaFb >= 11))
          (showString b1_aaFc)
    showsPrec
      a_aaFd
      (Tag b1_aaFe b2_aaFf)
      = showParen
          ((a_aaFd >= 11))
          ((.)
             (showString "Tag ")
             ((.)
                (showsPrec 11 b1_aaFe)
                ((.) showSpace (showsPrec 11 b2_aaFf))))
    showsPrec
      a_aaF8
      (Mul b1_aaF9 b2_aaFa)
      = showParen
          ((a_aaF8 >= 11))
          ((.)
             (showString "Mul ")
             ((.)
                (showsPrec 11 b1_aaF9)
                ((.) showSpace (showsPrec 11 b2_aaFa))))
