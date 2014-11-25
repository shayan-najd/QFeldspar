module Examples.Power.QDSL where
import Prelude
import QFeldspar.QDSL hiding (div,Int)

power :: Int -> Qt (Float -> Float)
power n =
  if n < 0 then
    [|| \x -> if x == 0 then 0 else 1 / ($$(power (-n)) x) ||]
  else if n == 0 then
    [|| \ _x -> 1 ||]
  else if even n then
    [|| \x -> $$sqr ($$(power (n `div` 2)) x) ||]
  else
    [|| \x -> x * ($$(power (n-1)) x) ||]

sqr  ::  Qt (Float -> Float)
sqr = [|| \ y -> y * y ||]

power' :: Int -> Qt (Float -> Maybe Float)
power' n =
  if n < 0 then
    [|| \x ->  if x == 0 then Nothing else
                 do y <- $$(power' (-n)) x; return (1 / y) ||]
  else if n == 0 then
    [|| \ _x -> return 1 ||]
  else if even n then
    [|| \x -> do y <- $$(power' (n `div` 2)) x; return ($$sqr y) ||]
  else
    [|| \x -> do y <- $$(power' (n-1)) x; return (x * y) ||]

power''      ::  Int -> Qt (Float -> Float)
power'' n = [|| \ x -> maybe 0 (\y -> y) ($$(power' n) x)||]
