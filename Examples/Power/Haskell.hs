module Examples.Power.Haskell where
import Prelude
power :: Int -> Float -> Float
power n x =
  if n < 0 then
    if x == 0 then 0 else 1 / power (-n) x
  else if n == 0 then
    1
  else if even n then
    sqr (power (n `div` 2) x)
  else
    x * power (n-1) x

sqr    ::  Float -> Float
sqr x  =   x * x

{-
  float main (float u) {
    if (u == 0) {
      return 0;
    } else {
      float v = u * 1;
      float w = u * (v * v);
      return 1 / (w * w);
    }
  }
-}

power' ::  Int -> Float -> Maybe Float
power' n x =
  if n < 0 then
    if x == 0 then Nothing else do y <- power' (-n) x; return (1 / y)
  else if n == 0 then
    return 1
  else if even n then
    do y <- power' (n `div` 2) x; return (sqr y)
  else
    do y <- power' (n-1) x; return (x * y)

power''      ::  Int -> Float -> Float
power'' n x  =   maybe 0 (\y -> y) (power' n x)
