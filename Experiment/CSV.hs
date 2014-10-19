module Experiment.CSV where

import Prelude
import System.Environment
import Text.Printf
infixl 4 </>
infixl 4 <|>
infixl 4 <@>
infixl 4 <->

(</>) :: String -> String -> String
d </> f = d ++ "/" ++ f

(<|>) :: String -> String -> String
s1 <|> s2 = s1 ++ "," ++ s2

(<@>) :: String -> String -> String
s1 <@> s2 = s1 ++ "\n" ++ s2

(<->) :: String -> String -> String
s1 <-> s2 = s1 ++ ",," ++ s2

main :: IO()
main = do
 [d] <- getArgs
 f   <- readFile (d </> "result")
 let ls   = (read f):: [(String,Float)]
     lk s = maybe "--" (printf "%.2f") (lookup s ls)
 writeFile (init d ++ ".csv") $
   "(QMiniFeldspar With CSE)"<|>"HC"                  <|> "HR"                 <|>"CC"                    <|> "CR"                 <->
   "(MiniFeldspar With CSE)" <|>"HC"                  <|> "HR"                 <|>"CC"                    <|> "CR"                 <@>
   "IPGray"         <|> lk "IPGrayQDSLTrueHC"    <|> lk "IPGrayQDSLTrueHR"    <|> lk "IPGrayQDSLTrueCC"      <|> lk "IPGrayQDSLTrueCR"    <->
   "IPGray"         <|> lk "IPGrayCDSLTrueHC"    <|> lk "IPGrayCDSLTrueHR"    <|> lk "IPGrayCDSLTrueCC"      <|> lk "IPGrayCDSLTrueCR"    <@>
   "IPBW"           <|> lk "IPBWQDSLTrueHC"      <|> lk "IPBWQDSLTrueHR"      <|> lk "IPBWQDSLTrueCC"        <|> lk "IPBWQDSLTrueCR"      <->
   "IPBW"           <|> lk "IPBWCDSLTrueHC"      <|> lk "IPBWCDSLTrueHR"      <|> lk "IPBWCDSLTrueCC"        <|> lk "IPBWCDSLTrueCR"      <@>
   "FFT"            <|> lk "FFTQDSLTrueHC"       <|> lk "FFTQDSLTrueHR"       <|> lk "FFTQDSLTrueCC"         <|> lk "FFTQDSLTrueCR"       <->
   "FFT"            <|> lk "FFTCDSLTrueHC"       <|> lk "FFTCDSLTrueHR"       <|> lk "FFTCDSLTrueCC"         <|> lk "FFTCDSLTrueCR"       <@>
   "CRC"            <|> lk "CRCQDSLTrueHC"       <|> lk "CRCQDSLTrueHR"       <|> lk "CRCQDSLTrueCC"         <|> lk "CRCQDSLTrueCR"       <->
   "CRC"            <|> lk "CRCCDSLTrueHC"       <|> lk "CRCCDSLTrueHR"       <|> lk "CRCCDSLTrueCC"         <|> lk "CRCCDSLTrueCR"       <@>
   "Windowing"      <|> lk "WindowingQDSLTrueHC" <|> lk "WindowingQDSLTrueHR" <|> lk "WindowingQDSLTrueCC"   <|> lk "WindowingQDSLTrueCR" <->
   "Windowing"      <|> lk "WindowingCDSLTrueHC" <|> lk "WindowingCDSLTrueHR" <|> lk "WindowingCDSLTrueCC"   <|> lk "WindowingCDSLTrueCR" <@>
   "(QMiniFeldspar Without CSE)"<|>"HC"                  <|> "HR"                 <|>"CC"                    <|> "CR"                 <->
   "(MiniFeldspar Without CSE)" <|>"HC"                  <|> "HR"                 <|>"CC"                    <|> "CR"                 <@>
   "IPGray"         <|> lk "IPGrayQDSLFalseHC"    <|> lk "IPGrayQDSLFalseHR"    <|> lk "IPGrayQDSLFalseCC"      <|> lk "IPGrayQDSLFalseCR"    <->
   "IPGray"         <|> lk "IPGrayCDSLFalseHC"    <|> lk "IPGrayCDSLFalseHR"    <|> lk "IPGrayCDSLFalseCC"      <|> lk "IPGrayCDSLFalseCR"    <@>
   "IPBW"           <|> lk "IPBWQDSLFalseHC"      <|> lk "IPBWQDSLFalseHR"      <|> lk "IPBWQDSLFalseCC"        <|> lk "IPBWQDSLFalseCR"      <->
   "IPBW"           <|> lk "IPBWCDSLFalseHC"      <|> lk "IPBWCDSLFalseHR"      <|> lk "IPBWCDSLFalseCC"        <|> lk "IPBWCDSLFalseCR"      <@>
   "FFT"            <|> lk "FFTQDSLFalseHC"       <|> lk "FFTQDSLFalseHR"       <|> lk "FFTQDSLFalseCC"         <|> lk "FFTQDSLFalseCR"       <->
   "FFT"            <|> lk "FFTCDSLFalseHC"       <|> lk "FFTCDSLFalseHR"       <|> lk "FFTCDSLFalseCC"         <|> lk "FFTCDSLFalseCR"       <@>
   "CRC"            <|> lk "CRCQDSLFalseHC"       <|> lk "CRCQDSLFalseHR"       <|> lk "CRCQDSLFalseCC"         <|> lk "CRCQDSLFalseCR"       <->
   "CRC"            <|> lk "CRCCDSLFalseHC"       <|> lk "CRCCDSLFalseHR"       <|> lk "CRCCDSLFalseCC"         <|> lk "CRCCDSLFalseCR"       <@>
   "Windowing"      <|> lk "WindowingQDSLFalseHC" <|> lk "WindowingQDSLFalseHR" <|> lk "WindowingQDSLFalseCC"   <|> lk "WindowingQDSLFalseCR" <->
   "Windowing"      <|> lk "WindowingCDSLFalseHC" <|> lk "WindowingCDSLFalseHR" <|> lk "WindowingCDSLFalseCC"   <|> lk "WindowingCDSLFalseCR"

