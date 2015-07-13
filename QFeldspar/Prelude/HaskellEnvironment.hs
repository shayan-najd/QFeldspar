module QFeldspar.Prelude.HaskellEnvironment where

import QFeldspar.MyPrelude hiding (realPart,imagPart,mod)

import qualified QFeldspar.Environment.Scoped  as ES
import qualified QFeldspar.Environment.Plain   as EP
import qualified Language.Haskell.TH.Syntax    as TH
import qualified QFeldspar.Nat.ADT             as NA
import QFeldspar.Singleton hiding (sin)
import QFeldspar.Environment.Conversion ()
import QFeldspar.Expression.Utils.TemplateHaskell
import QFeldspar.Conversion
import qualified QFeldspar.Prelude.Haskell as PH

infixr 5 <+>
(<+>) :: t -> ES.Env n t -> ES.Env (NA.Suc n) t
(<+>) = ES.Ext

esTH :: ES.Env (Len Prelude) TH.Name
esTH = fmap stripNameSpace esTH'

epTH :: EP.Env TH.Name
epTH = frmRgtZro (cnv (esTH , ()))

type Prelude = [Complex Float -> Float,
                Complex Float -> Float,
                Word32 -> (Word32 -> Word32),
                Float -> (Float -> Float),
                Word32 -> (Word32 -> Word32),
                Word32 -> (Word32 -> Word32),
                Word32 -> (Word32 -> Word32),
                Word32 -> (Word32 -> Word32),
                Word32 -> (Word32 -> Word32),
                Word32 -> Word32,
                Word32 -> Float,
                Float -> Complex Float,
                Word32 -> Word32,
                Float -> Float,
                Ary Word32,
                Float -> Float,
                Float -> Float,
                Float -> Float -> Float,
                Float -> Word32,
                Word32 -> Word32 -> Word32]

esTH' :: ES.Env (Len Prelude) TH.Name
esTH' = 'realPart
    <+> 'imagPart
    <+> 'divWrd
    <+> 'divFlt
    <+> 'andWrd
    <+> 'orWrd
    <+> 'xorWrd
    <+> 'shrWrd
    <+> 'shlWrd
    <+> 'cmpWrd
    <+> 'i2f
    <+> 'cis
    <+> 'ilog2
    <+> 'sqrtFlt
    <+> 'hshTbl
    <+> 'sin
    <+> 'cos
    <+> 'atan2
    <+> 'round
    <+> 'mod
    <+> ES.Emp

realPart :: Complex Float -> Float
realPart = PH.realPart

imagPart :: Complex Float -> Float
imagPart = PH.imagPart

divWrd :: Word32 -> (Word32 -> Word32)
divWrd =  PH.div

divFlt :: Float -> (Float -> Float)
divFlt = (PH./)

andWrd :: Word32 -> (Word32 -> Word32)
andWrd = (PH..&.)

orWrd :: Word32 -> (Word32 -> Word32)
orWrd = (PH..|.)

xorWrd :: Word32 -> (Word32 -> Word32)
xorWrd = PH.xor

shrWrd :: Word32 -> (Word32 -> Word32)
shrWrd = PH.shfRgt

shlWrd :: Word32 -> (Word32 -> Word32)
shlWrd = PH.shfLft

cmpWrd :: Word32 -> Word32
cmpWrd = PH.complement

i2f :: Word32 -> Float
i2f = PH.i2f

cis :: Float -> Complex Float
cis = PH.cis

ilog2 :: Word32 -> Word32
ilog2 = PH.ilog2

sqrtFlt :: Float -> Float
sqrtFlt = PH.sqrt

hshTbl :: Ary Word32
hshTbl = PH.hashTable

sin :: Float -> Float
sin = PH.sin

cos :: Float -> Float
cos = PH.cos

atan2 :: Float -> Float -> Float
atan2 = PH.atan2

round :: Float -> Word32
round = PH.round

mod :: Word32 -> Word32 -> Word32
mod = PH.mod
