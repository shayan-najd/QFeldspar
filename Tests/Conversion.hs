module Tests.Conversion where

import QFeldspar.MyPrelude

import qualified Language.Haskell.TH.Syntax              as TH
import qualified QFeldspar.Expression.ADTUntypedNamed     as AUN
import qualified QFeldspar.Expression.ADTUntypedDebruijn  as AUD
import qualified QFeldspar.Expression.GADTTyped           as GTD
import qualified QFeldspar.Expression.GADTFirstOrder      as GFO
import qualified QFeldspar.Expression.GADTHigherOrder     as GHO
import qualified QFeldspar.Expression.MiniFeldspar      as MFS
import qualified QFeldspar.Expression.ADTValue            as FAV
import qualified QFeldspar.Expression.GADTValue           as FGV
import qualified Tests.TemplateHaskell     as TH
import qualified Tests.ADTUntypedNamed     as AUN
import qualified Tests.ADTUntypedDebruijn  as AUD
import qualified Tests.GADTTyped           as GTD
import qualified Tests.GADTFirstOrder      as GFO
import qualified Tests.GADTHigherOrder     as GHO
import qualified Tests.MiniFeldspar        as MFS
import qualified QFeldspar.Type.ADT                       as TA
import qualified QFeldspar.Type.GADT                      as TG
import qualified QFeldspar.Environment.Map                         as EM
import qualified QFeldspar.Environment.Plain                       as EP
import qualified QFeldspar.Environment.Scoped                      as ES
import qualified QFeldspar.Environment.Typed                       as ET
import qualified QFeldspar.Normalisation  as NGFO
import QFeldspar.Conversion
import QFeldspar.Variable.Conversion                     ()
import QFeldspar.Environment.Conversion                  ()
import QFeldspar.Type.Conversion                ()
import QFeldspar.Expression.Conversion          ()
import qualified QFeldspar.Nat.ADT as NA
import QFeldspar.Expression.Utils.TemplateHaskell

type One    = NA.Suc NA.Zro
type Add    = Word32 -> Word32 -> Word32
type EnvAdd = '[Add]

typAddG :: TG.Typ Add
typAddG = (TG.Arr TG.Wrd (TG.Arr TG.Wrd TG.Wrd))

envAddTypG :: ET.Env TG.Typ EnvAdd
envAddTypG =  ET.Ext typAddG ET.Emp

vec :: ES.Env One TH.Name
vec = ES.Ext (stripNameSpace 'TH.add) ES.Emp

envAddValG :: ET.Env FGV.Exp EnvAdd
envAddValG = ET.Ext (FGV.Exp (+)
                       :: FGV.Exp (Word32 -> Word32 -> Word32))
             ET.Emp

envAddValV :: ES.Env One FAV.Exp
envAddValV = ES.Ext (FAV.lft ((+) :: Word32 -> Word32 -> Word32)) ES.Emp

envAddValA :: EP.Env FAV.Exp
envAddValA = (FAV.lft ((+) :: Word32 -> Word32 -> Word32)) : []

envAddValM :: EM.Env TH.Name FAV.Exp
envAddValM = (stripNameSpace 'TH.add , FAV.lft ((+) :: Word32 -> Word32 -> Word32)) : []

cnvMFS :: Cnv (e , ET.Env TG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
               (GFO.Exp EnvAdd '[] Word32) => e -> Word32 -> Bool
cnvMFS e j = case runNamM
              (do e'   :: GFO.Exp EnvAdd '[] Word32 <- cnv (e , envAddTypG
                                                            , vec)
                  let e'' = NGFO.nrm e'
                  e''' :: MFS.Exp EnvAdd Word32 <- cnv (e'' , envAddTypG
                                                     ,vec)
                  curry cnv e''' envAddValG) of
           Rgt (FGV.Exp i) -> i == j
           _     -> False

cnvGHO :: Cnv (e , ET.Env TG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (GHO.Exp EnvAdd Word32) => e -> Word32 -> Bool
cnvGHO e j = case runNamM
              (do e' :: GHO.Exp EnvAdd  Word32 <- cnv (e , envAddTypG,vec)
                  curry cnv e' envAddValG) of
           Rgt (FGV.Exp i) -> i == j
           _     -> False

cnvGFO :: Cnv (e , ET.Env TG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (GFO.Exp EnvAdd '[] Word32) => e -> Word32 -> Bool
cnvGFO e j = case runNamM
              (do e' :: GFO.Exp EnvAdd '[] Word32 <- cnv (e , envAddTypG ,vec)
                  cnv (e' , (envAddValG ,ET.Emp :: ET.Env FGV.Exp '[]))) of
           Rgt (FGV.Exp i) -> i == j
           _               -> False

cnvGTD :: Cnv (e , ET.Env TG.Typ EnvAdd , ES.Env One TH.Name)
           (GTD.Exp One NA.Zro TA.Typ) => e -> Word32 -> Bool
cnvGTD e j = case runNamM
              (do e' :: GTD.Exp One NA.Zro TA.Typ <- cnv (e , envAddTypG , vec)
                  cnv (e' , (envAddValV,ES.Emp :: ES.Env NA.Zro FAV.Exp))) of
           Rgt (FAV.colft -> Rgt i) -> i == j
           _                        -> False

cnvAUD :: Cnv (e , ET.Env TG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           AUD.Exp => e -> Word32 -> Bool
cnvAUD e j = case runNamM
              (do e' :: AUD.Exp <- cnv (e , envAddTypG , vec)
                  cnv (e' , (envAddValA , [] :: EP.Env FAV.Exp))) of
           Rgt (FAV.colft -> Rgt i) -> i == j
           _                        -> False

cnvAUN :: Cnv (e , ET.Env TG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (AUN.Exp TH.Name) => e -> Word32 -> Bool
cnvAUN e j = case runNamM
              (do e' :: AUN.Exp TH.Name <- cnv (e , envAddTypG , vec)
                  cnv (e' , (envAddValM,[] :: EM.Env TH.Name FAV.Exp))) of
           Rgt (FAV.colft -> Rgt i) -> i == j
           _                        -> False

test :: Bool
test = cnvAUN TH.four   4 && cnvAUN AUN.four 4 &&

       cnvAUD TH.four   4 && cnvAUD AUN.four 4 && cnvAUD AUD.four 4 &&

       cnvGTD TH.four   4 && cnvGTD AUN.four 4 && cnvGTD AUD.four 4 &&
       cnvGTD GTD.four 4 &&

       cnvGFO TH.four   4 && cnvGFO AUN.four 4 && cnvGFO AUD.four 4 &&
       cnvGFO GTD.four 4 && cnvGFO GFO.four 4 && cnvGFO GHO.four 4 &&

       cnvGHO TH.four   4 && cnvGHO AUN.four 4 && cnvGHO AUD.four 4 &&
       cnvGHO GTD.four 4 && cnvGHO GFO.four 4 &&
       cnvGHO GHO.four 4 &&

       cnvMFS TH.four   4 && cnvMFS AUN.four 4  && cnvMFS AUD.four 4 &&
       cnvMFS GTD.four 4 && cnvMFS GFO.four 4 &&
       cnvMFS GHO.four 4 && cnvMFS MFS.four 4
