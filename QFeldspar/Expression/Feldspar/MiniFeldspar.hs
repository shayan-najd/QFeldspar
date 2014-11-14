module QFeldspar.Expression.Feldspar.MiniFeldspar
      (Exp(..)) where

import QFeldspar.MyPrelude
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
