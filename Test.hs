module Test where

import QFeldspar.MyPrelude

import qualified Tests.ADTUntypedNamed     as FAUN
import qualified Tests.ADTUntypedDebruijn  as FAUD
import qualified Tests.GADTUntypedDebruijn as FGUD
import qualified Tests.GADTTyped           as FGTD
import qualified Tests.GADTFirstOrder      as FGFO
import qualified Tests.GADTHigherOrder     as FGHO
import qualified Tests.MiniFeldspar      as FMWS
import qualified Tests.Conversion          as FCNV

import qualified Examples.CRC.CDSL ()
import qualified Examples.CRC.QDSL ()
import qualified Examples.FFT.CDSL ()
import qualified Examples.FFT.QDSL ()
import qualified Examples.IPBW.CDSL ()
import qualified Examples.IPBW.QDSL ()
import qualified Examples.IPGray.CDSL ()
import qualified Examples.IPGray.QDSL ()
import qualified Examples.Windowing.CDSL ()
import qualified Examples.Windowing.QDSL ()

import QFeldspar.Expression.Feldspar.Conversions.Evaluation.ADTValue ()

main :: IO ()
main = print (if FAUN.test  && FAUD.test  && FGUD.test &&
                 FGTD.test  && FGFO.test  && FGHO.test &&
                 FMWS.test  && FCNV.test
              then "Pass!"
              else "Fail!")

-- Todo:
-- * Check for exotic terms
-- * Weakening of HOAS
--    + getting rid of Tmp
-- * Bidirectional translations
-- * Using Type classes to do lifting and colifting for ADTValue and GADTValue
--   to reuse Vanilla Prelude
-- * check for all exhaustive partterns and transform them
-- * Conversion of FGHO (x :-> y) ~> (FMWS x -> FMWS y)
-- * Free Fusion for Church / Ahman's Containers
-- * Supporting F
-- * Scope Proofing Quotations (e.g. Sam's misunderstanding) [EncodingTypes.txt]
-- * Support for Syntactic Suggar in Quotations (e.g. use TH-Desugar)
-- * Add sqrt and memorize (for Float) to Preludes that do not have it
-- * Write the code required for memorize
-- * Add support for partial type annotation *in* the quoted language
-- * Use macros for polymorphic datatypes
-- * Generate polymorphic datatypes
-- * Check all external imports are only via MyPrelude
-- * Shift Whole Pipline to the compile time!
-- * Use let in FFT and CRC
-- * Use eta