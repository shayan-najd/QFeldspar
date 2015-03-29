module Examples.Power.CDSL where
import Prelude
import QFeldspar.CDSL hiding (Int)

power :: Int -> Dp Float -> Dp Float
power n xx =
  if n < 0 then
    share xx (\ x ->
    (x ==. 0) ? (0 ,  1 / power (-n) x))
  else if n == 0 then
    1
  else if even n then
    sqr (power (div n 2) xx)
  else
    share xx (\ x ->
    x * power (n-1) x)

sqr ::  Dp Float -> Dp Float
sqr yy  = share yy (\ y ->
          y * y)

power' :: Int -> Dp Float -> Opt (Dp Float)
power' n xx  =
  if n < 0 then
    share xx (\ x ->
    (x ==. 0) ? (none, do y <- power' (-n) x; return (1 / y)))
  else if n == 0 then
    return 1
  else if even n then
    do y <- power' (div n 2) xx; return (sqr y)
  else
    share xx (\ x ->
    do y <- power' (n - 1) x; return (x * y))

power'' ::  Int -> Dp Float -> Dp Float
power'' n x  =  option 0 (\y -> y) (power' n x)
