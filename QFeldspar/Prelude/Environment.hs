module QFeldspar.Prelude.Environment where

import QFeldspar.MyPrelude hiding (realPart,imagPart,mod)

import qualified QFeldspar.Expression.ADTValue  as FAV
import qualified QFeldspar.Expression.GADTValue as FGV
import qualified Language.Haskell.TH            as TH
import QFeldspar.Nat.TH
import qualified QFeldspar.Nat.ADT as NA

import qualified QFeldspar.Type.GADT as TG

import qualified QFeldspar.Environment.Map    as EM
import qualified QFeldspar.Environment.Plain  as EP
import qualified QFeldspar.Environment.Scoped as ES
import qualified QFeldspar.Environment.Typed  as ET
import QFeldspar.Conversion
import QFeldspar.Environment.Conversion ()
import QFeldspar.Variable.Typed
import qualified QFeldspar.Singleton as S
import QFeldspar.Expression.Utils.TemplateHaskell
import QFeldspar.Prelude.HaskellEnvironment

infixr 5 <:>
(<:>) :: tf t -> ET.Env tf e -> ET.Env tf (t ': e)
(<:>) = ET.Ext

-----------------------------------------------------------------------
-- ETTG
-----------------------------------------------------------------------

etTG :: ET.Env TG.Typ Prelude
etTG = S.sin

-----------------------------------------------------------------------
-- ESString
-----------------------------------------------------------------------

esString :: ES.Env (S.Len Prelude) String
esString = fmap TH.nameBase esTH

-----------------------------------------------------------------------
-- Var
-----------------------------------------------------------------------

$(sequence (join
 [[do t <- [t| Var Prelude (ET.Get Prelude $(natT i "NA.")) |]
      return (TH.SigD n t)
 , do e <- nat i ""
      return (TH.ValD (TH.VarP n) (TH.NormalB e) [])]
  | (i , nHsk) <- zip [0..] epTH ,
    let nB = TH.nameBase nHsk,
    let n  = stripNameSpace (TH.mkName (nB ++ "Var"))])
 )

-----------------------------------------------------------------------
-- FAV
-----------------------------------------------------------------------

$(sequence (join
 [[TH.SigD n <$> [t| FAV.Exp |]
 , do e <- [| FAV.toExp $(TH.varE nHsk) |]
      return (TH.ValD (TH.VarP n) (TH.NormalB e) [])]
  | nHsk <- epTH ,
    let nB = TH.nameBase nHsk,
    let n  = stripNameSpace (TH.mkName (nB ++ "FAV"))])
 )

-----------------------------------------------------------------------
-- etFGV
-----------------------------------------------------------------------

etFGV :: ET.Env FGV.Exp Prelude
etFGV = $(return (foldr
            (\ a b ->
               TH.InfixE (Just (TH.AppE (TH.ConE 'FGV.Exp)
                     (TH.VarE a))) (TH.ConE 'ET.Ext) (Just b))
            (TH.ConE 'ET.Emp)
            epTH))

-----------------------------------------------------------------------
-- ESFAV
-----------------------------------------------------------------------

esFAV :: ES.Env (S.Len Prelude) FAV.Exp
esFAV = $(return (foldr
            (\ a b ->
               TH.InfixE (Just (TH.VarE a)) (TH.ConE 'ES.Ext) (Just b))
            (TH.ConE 'ES.Emp)
            (fmap (stripNameSpace . TH.mkName . (++ "FAV")
                   .TH.nameBase) epTH)))

-----------------------------------------------------------------------
-- EMTHFAV
-----------------------------------------------------------------------

emTHFAV :: EM.Env TH.Name FAV.Exp
emTHFAV = let is :: EP.Env TH.Name = frmRgtZro (cnv (esTH  , ()))
              es :: EP.Env FAV.Exp = frmRgtZro (cnv (esFAV , ()))
          in zip is es
