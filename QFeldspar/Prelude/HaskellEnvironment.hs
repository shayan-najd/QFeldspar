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

type Prelude = [Cmx -> Flt          ,
                Cmx -> Flt          ,
                Int -> (Int -> Int) ,
                Flt -> (Flt -> Flt) ,
                Int -> (Int -> Int) ,
                Int -> (Int -> Int) ,
                Int -> (Int -> Int) ,
                Int -> (Int -> Int) ,
                Int -> (Int -> Int) ,
                Int -> Int          ,
                Int -> Flt          ,
                Flt -> Cmx          ,
                Int -> Int          ,
                Flt -> Flt          ,
                Ary Int             ]

esTH' :: ES.Env (Len Prelude) TH.Name
esTH' = 'realPart
    <+> 'imagPart
    <+> 'divInt
    <+> 'divFlt
    <+> 'andInt
    <+> 'orInt
    <+> 'xorInt
    <+> 'shrInt
    <+> 'shlInt
    <+> 'cmpInt
    <+> 'i2f
    <+> 'cis
    <+> 'ilog2
    <+> 'sqrtFlt
    <+> 'hshTbl
    <+> ES.Emp

realPart :: Cmx -> Flt
realPart = MP.realPart

imagPart :: Cmx -> Flt
imagPart = MP.imagPart

divInt :: Int -> (Int -> Int)
divInt =  div

divFlt :: Flt -> (Flt -> Flt)
divFlt = (/)

andInt :: Int -> (Int -> Int)
andInt = (.&.)

orInt :: Int -> (Int -> Int)
orInt = (.|.)

xorInt :: Int -> (Int -> Int)
xorInt = xor

shrInt :: Int -> (Int -> Int)
shrInt = shfRgt

shlInt :: Int -> (Int -> Int)
shlInt = shfLft

cmpInt :: Int -> Int
cmpInt = complement

i2f :: Int -> Flt
i2f = MP.i2f

cis :: Flt -> Cmx
cis = MP.cis

ilog2 :: Int -> Int
ilog2 = MP.ilog2

sqrtFlt :: Flt -> Flt
sqrtFlt = sqrt

hshTbl :: Ary Int
hshTbl = hashTable
