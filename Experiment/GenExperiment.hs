module Experiment.GenExperiment where
import Prelude
import Data.Char

data Frnt = CDSL
          | QDSL
          deriving Show

lower :: String -> String
lower = fmap toLower

main :: IO ()
main = sequence_ $
       flip map  ([(e,f,s)
                 | e <- ["CRC","FFT","IPBW","IPGray","Windowing"]
                 , f <- [CDSL,QDSL]
                 , s <- [False,True]])
 (\ (e,f,s) ->
   let ff = "import Prelude\n" ++
            "import Experiment.Common\n"++
            "import QFeldspar." ++ show f ++ "\n" ++
            "import Examples." ++e++ "." ++ show f ++ "\n" ++
            "main = writeFile \"Experiment/" ++ e ++ show f ++ show s ++".c\""
                   ++ " (header++(compileF "++show s++" "++lower e
                          ++")++loader"++e++")"
   in writeFile ("Experiment/run"++e++show f++show s++".hs") ff
  )