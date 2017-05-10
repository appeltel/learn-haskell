-- Mathy stuff for simple programs
module Mathy
( fac,
  fib
) where


fac :: Integer -> Integer
fac n
    | n < 0 = error "negative number"
    | n == 0 = 1
    | otherwise = n * fac (n - 1)

fib :: Integer -> Integer
fib n
    | n < 1 = error "non-positive number"
    | n == 1 = 1
    | n == 2 = 1
    | otherwise = (fib (n - 1)) + (fib (n - 2))
