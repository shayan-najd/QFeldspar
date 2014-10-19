module Tests.Conversion where

import QFeldspar.MyPrelude

import qualified Language.Haskell.TH.Syntax              as TH
import qualified QFeldspar.Expression.Feldspar.ADTUntypedNamed     as FAUN
import qualified QFeldspar.Expression.Feldspar.ADTUntypedDebruijn  as FAUD
import qualified QFeldspar.Expression.Feldspar.GADTUntypedDebruijn as FGUD
import qualified QFeldspar.Expression.Feldspar.GADTTyped           as FGTD
import qualified QFeldspar.Expression.Feldspar.GADTFirstOrder      as FGFO
import qualified QFeldspar.Expression.Feldspar.GADTHigherOrder     as FGHO
import qualified QFeldspar.Expression.Feldspar.MiniWellScoped      as FMWS
import qualified QFeldspar.Expression.Feldspar.ADTValue            as FAV
import qualified QFeldspar.Expression.Feldspar.GADTValue           as FGV

import qualified Tests.TemplateHaskell     as TH
import qualified Tests.ADTUntypedNamed     as FAUN
import qualified Tests.ADTUntypedDebruijn  as FAUD
import qualified Tests.GADTUntypedDebruijn as FGUD
import qualified Tests.GADTTyped           as FGTD
import qualified Tests.GADTFirstOrder      as FGFO
import qualified Tests.GADTHigherOrder     as FGHO
import qualified Tests.MiniWellScoped      as FMWS ()

import qualified QFeldspar.Type.Feldspar.ADT                       as TFA
import qualified QFeldspar.Type.Feldspar.GADT                      as TFG

import qualified QFeldspar.Environment.Map                         as EM
import qualified QFeldspar.Environment.Plain                       as EP
import qualified QFeldspar.Environment.Scoped                      as ES
import qualified QFeldspar.Environment.Typed                       as ET

import QFeldspar.Normalization
import QFeldspar.Normalization.Feldspar.GADTHigherOrder  ()

import QFeldspar.Conversion
import QFeldspar.Variable.Conversion                     ()
import QFeldspar.Environment.Conversion                  ()
import QFeldspar.Type.Feldspar.Conversion                ()
import QFeldspar.Expression.TemplateHaskell.Conversion   ()
import QFeldspar.Expression.Feldspar.Conversion          ()

import qualified QFeldspar.Nat.ADT as NA

type One    = NA.Suc NA.Zro
type Add    = Arr Int (Arr Int Int)
type EnvAdd = Add ': '[]

typAddG :: TFG.Typ Add
typAddG = (TFG.Arr TFG.Int (TFG.Arr TFG.Int TFG.Int))

envAddTypG :: ET.Env TFG.Typ EnvAdd
envAddTypG =  ET.Ext typAddG ET.Emp

vec :: ES.Env One TH.Name
vec = ES.Ext '(+) ES.Emp

envAddValG :: ET.Env FGV.Exp EnvAdd
envAddValG = ET.Ext (FGV.Exp (+)
                       :: FGV.Exp (Arr Int (Arr Int Int)))
             ET.Emp

envAddValV :: ES.Env One FAV.Exp
envAddValV = ES.Ext FAV.addV ES.Emp

envAddValA :: EP.Env FAV.Exp
envAddValA = FAV.addV : []

envAddValM :: EM.Env TH.Name FAV.Exp
envAddValM = ('(+) , FAV.addV) : []

cnvFMWS :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
               (FGHO.Exp EnvAdd Int) => e -> Int -> Bool
cnvFMWS e j = case (do e'   :: FGHO.Exp EnvAdd Int <- cnv (e , envAddTypG
                                                              , vec)
                       let e'' = nrm e'
                       e''' :: FMWS.Exp EnvAdd Int <- cnv (e'' , envAddTypG
                                                              ,vec)
                       curry cnv e''' envAddValG) of
           Rgt (FGV.Exp i) -> i == j
           _     -> False

cnvFGHO :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (FGHO.Exp EnvAdd Int) => e -> Int -> Bool
cnvFGHO e j = case (do e' :: FGHO.Exp EnvAdd  Int <- cnv (e , envAddTypG,vec)
                       curry cnv e' envAddValG) of
           Rgt (FGV.Exp i) -> i == j
           _     -> False

cnvFGFO :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (FGFO.Exp EnvAdd Int) => e -> Int -> Bool
cnvFGFO e j = case (do e' :: FGFO.Exp EnvAdd Int <- cnv (e , envAddTypG ,vec)
                       curry cnv e' envAddValG) of
           Rgt (FGV.Exp i) -> i == j
           _             -> False

cnvFGTD :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (FGTD.Exp One TFA.Typ) => e -> Int -> Bool
cnvFGTD e j = case (do e' :: FGTD.Exp One TFA.Typ <- cnv (e , envAddTypG , vec)
                       curry cnv e' envAddValV) of
           Rgt (FAV.ConI i) -> i == j
           _                -> False

cnvFGUD :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (FGUD.Exp One) => e -> Int -> Bool
cnvFGUD e j = case (do e' :: FGUD.Exp One <- cnv (e , envAddTypG , vec)
                       curry cnv e' envAddValV) of
           Rgt (FAV.ConI i) -> i == j
           _                -> False

cnvFAUD :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           FAUD.Exp => e -> Int -> Bool
cnvFAUD e j = case (do e' :: FAUD.Exp <- cnv (e , envAddTypG , vec)
                       curry cnv e' envAddValA) of
           Rgt (FAV.ConI i) -> i == j
           _                -> False

cnvFAUN :: Cnv (e , ET.Env TFG.Typ EnvAdd , ES.Env (NA.Suc NA.Zro) TH.Name)
           (FAUN.Exp TH.Name) => e -> Int -> Bool
cnvFAUN e j = case (do e' :: FAUN.Exp TH.Name <- cnv (e , envAddTypG , vec)
                       curry cnv e' envAddValM) of
           Rgt (FAV.ConI i) -> i == j
           _                -> False

test :: Bool
test = cnvFAUN TH.four   4 && cnvFAUN FAUN.four 4 &&

       cnvFAUD TH.four   4 && cnvFAUD FAUN.four 4 && cnvFAUD FAUD.four 4 &&

       cnvFGUD TH.four   4 && cnvFGUD FAUN.four 4 && cnvFGUD FAUD.four 4 &&
       cnvFGUD FGUD.four 4 &&

       cnvFGTD TH.four   4 && cnvFGTD FAUN.four 4 && cnvFGTD FAUD.four 4 &&
       cnvFGTD FGUD.four 4 && cnvFGTD FGTD.four 4 &&

       cnvFGFO TH.four   4 && cnvFGFO FAUN.four 4 && cnvFGFO FAUD.four 4 &&
       cnvFGFO FGUD.four 4 && cnvFGFO FGTD.four 4 && cnvFGFO FGFO.four 4 &&

       cnvFGHO TH.four   4 && cnvFGHO FAUN.four 4 && cnvFGHO FAUD.four 4 &&
       cnvFGHO FGUD.four 4 && cnvFGHO FGTD.four 4 && cnvFGHO FGFO.four 4 &&
       cnvFGHO FGHO.four 4 &&

       cnvFMWS TH.four   4 && cnvFMWS FAUN.four 4 && cnvFMWS FAUD.four 4 &&
       cnvFMWS FGUD.four 4 && cnvFMWS FGTD.four 4 && cnvFMWS FGFO.four 4 &&
       cnvFMWS FGHO.four 4 -- && cnvFMWS FWMS.four 4
