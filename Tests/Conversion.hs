module Tests.Conversion where

import QFeldspar.MyPrelude

import qualified Language.Haskell.TH.Syntax              as TH
import qualified QFeldspar.Expression.ADTUntypedNamed     as FAUN
import qualified QFeldspar.Expression.ADTUntypedDebruijn  as FAUD
import qualified QFeldspar.Expression.GADTTyped           as FGTD
import qualified QFeldspar.Expression.GADTFirstOrder      as FGFO
import qualified QFeldspar.Expression.GADTHigherOrder     as FGHO
import qualified QFeldspar.Expression.MiniFeldspar      as FMWS
import qualified QFeldspar.Expression.ADTValue            as FAV
import qualified QFeldspar.Expression.GADTValue           as FGV
import qualified Tests.TemplateHaskell     as TH
import qualified Tests.ADTUntypedNamed     as FAUN
import qualified Tests.ADTUntypedDebruijn  as FAUD
import qualified Tests.GADTTyped           as FGTD
import qualified Tests.GADTFirstOrder      as FGFO
import qualified Tests.GADTHigherOrder     as FGHO
import qualified Tests.MiniFeldspar        as FMWS
import qualified QFeldspar.Type.ADT                       as TFA
import qualified QFeldspar.Type.GADT                      as TFG
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

typAddG :: TFG.Typ Add
typAddG = (TFG.Arr TFG.Wrd (TFG.Arr TFG.Wrd TFG.Wrd))

envAddTypG :: ET.Env TFG.Typ EnvAdd
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

cnvFMWS :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
               (FGFO.Exp EnvAdd '[] Word32) => e -> Word32 -> Bool
cnvFMWS e j = case runNamM
              (do e'   :: FGFO.Exp EnvAdd '[] Word32 <- cnv (e , envAddTypG
                                                            , vec)
                  let e'' = NGFO.nrm e'
                  e''' :: FMWS.Exp EnvAdd Word32 <- cnv (e'' , envAddTypG
                                                     ,vec)
                  curry cnv e''' envAddValG) of
           Rgt (FGV.Exp i) -> i == j
           _     -> False

cnvFGHO :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (FGHO.Exp EnvAdd Word32) => e -> Word32 -> Bool
cnvFGHO e j = case runNamM
              (do e' :: FGHO.Exp EnvAdd  Word32 <- cnv (e , envAddTypG,vec)
                  curry cnv e' envAddValG) of
           Rgt (FGV.Exp i) -> i == j
           _     -> False

cnvFGFO :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (FGFO.Exp EnvAdd '[] Word32) => e -> Word32 -> Bool
cnvFGFO e j = case runNamM
              (do e' :: FGFO.Exp EnvAdd '[] Word32 <- cnv (e , envAddTypG ,vec)
                  cnv (e' , (envAddValG ,ET.Emp :: ET.Env FGV.Exp '[]))) of
           Rgt (FGV.Exp i) -> i == j
           _               -> False

cnvFGTD :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env One TH.Name)
           (FGTD.Exp One NA.Zro TFA.Typ) => e -> Word32 -> Bool
cnvFGTD e j = case runNamM
              (do e' :: FGTD.Exp One NA.Zro TFA.Typ <- cnv (e , envAddTypG , vec)
                  cnv (e' , (envAddValV,ES.Emp :: ES.Env NA.Zro FAV.Exp))) of
           Rgt (FAV.colft -> Rgt i) -> i == j
           _                        -> False

cnvFAUD :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           FAUD.Exp => e -> Word32 -> Bool
cnvFAUD e j = case runNamM
              (do e' :: FAUD.Exp <- cnv (e , envAddTypG , vec)
                  cnv (e' , (envAddValA , [] :: EP.Env FAV.Exp))) of
           Rgt (FAV.colft -> Rgt i) -> i == j
           _                        -> False

cnvFAUN :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (FAUN.Exp TH.Name) => e -> Word32 -> Bool
cnvFAUN e j = case runNamM
              (do e' :: FAUN.Exp TH.Name <- cnv (e , envAddTypG , vec)
                  cnv (e' , (envAddValM,[] :: EM.Env TH.Name FAV.Exp))) of
           Rgt (FAV.colft -> Rgt i) -> i == j
           _                        -> False

test :: Bool
test = cnvFAUN TH.four   4 && cnvFAUN FAUN.four 4 &&

       cnvFAUD TH.four   4 && cnvFAUD FAUN.four 4 && cnvFAUD FAUD.four 4 &&

       cnvFGTD TH.four   4 && cnvFGTD FAUN.four 4 && cnvFGTD FAUD.four 4 &&
       cnvFGTD FGTD.four 4 &&

       cnvFGFO TH.four   4 && cnvFGFO FAUN.four 4 && cnvFGFO FAUD.four 4 &&
       cnvFGFO FGTD.four 4 && cnvFGFO FGFO.four 4 && cnvFGFO FGHO.four 4 &&

       cnvFGHO TH.four   4 && cnvFGHO FAUN.four 4 && cnvFGHO FAUD.four 4 &&
       cnvFGHO FGTD.four 4 && cnvFGHO FGFO.four 4 &&
       cnvFGHO FGHO.four 4 &&

       cnvFMWS TH.four   4 && cnvFMWS FAUN.four 4  && cnvFMWS FAUD.four 4 &&
       cnvFMWS FGTD.four 4 && cnvFMWS FGFO.four 4 &&
       cnvFMWS FGHO.four 4 && cnvFMWS FMWS.four 4
