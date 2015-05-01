module QFeldspar.Prelude.HaskellEnvironment where

import QFeldspar.MyPrelude hiding (cis,realPart,imagPart,i2f,ilog2)
import qualified QFeldspar.MyPrelude as MP

import qualified QFeldspar.Environment.Scoped  as ES
import qualified QFeldspar.Environment.Plain   as EP
import qualified Language.Haskell.TH.Syntax    as TH
import qualified QFeldspar.Nat.ADT             as NA
import QFeldspar.Singleton
import QFeldspar.Environment.Conversion ()
import QFeldspar.Expression.Utils.TemplateHaskell
import QFeldspar.Conversion

infixr 5 <+>
(<+>) :: t -> ES.Env n t -> ES.Env (NA.Suc n) t
(<+>) = ES.Ext

esTH :: ES.Env (Len Prelude) TH.Name
esTH = fmap stripNameSpace esTH'

epTH :: EP.Env TH.Name
epTH = frmRgtZro (cnv (esTH , ()))

type Prelude = [Complex Float -> Float          ,
                Complex Float -> Float          ,
                Word32 -> (Word32 -> Word32) ,
                Float -> (Float -> Float) ,
                Word32 -> (Word32 -> Word32) ,
                Word32 -> (Word32 -> Word32) ,
                Word32 -> (Word32 -> Word32) ,
                Word32 -> (Word32 -> Word32) ,
                Word32 -> (Word32 -> Word32) ,
                Word32 -> Word32          ,
                Word32 -> Float          ,
                Float -> Complex Float          ,
                Word32 -> Word32          ,
                Float -> Float          ,
                Ary Word32             ]

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
    <+> ES.Emp

realPart :: Complex Float -> Float
realPart = MP.realPart

imagPart :: Complex Float -> Float
imagPart = MP.imagPart

divWrd :: Word32 -> (Word32 -> Word32)
divWrd =  div

divFlt :: Float -> (Float -> Float)
divFlt = (/)

andWrd :: Word32 -> (Word32 -> Word32)
andWrd = (.&.)

orWrd :: Word32 -> (Word32 -> Word32)
orWrd = (.|.)

xorWrd :: Word32 -> (Word32 -> Word32)
xorWrd = xor

shrWrd :: Word32 -> (Word32 -> Word32)
shrWrd = shfRgt

shlWrd :: Word32 -> (Word32 -> Word32)
shlWrd = shfLft

cmpWrd :: Word32 -> Word32
cmpWrd = complement

i2f :: Word32 -> Float
i2f = MP.i2f

cis :: Float -> Complex Float
cis = MP.cis

ilog2 :: Word32 -> Word32
ilog2 = MP.ilog2

sqrtFlt :: Float -> Float
sqrtFlt = sqrt

hshTbl :: Ary Word32
hshTbl = hashTable
