-- Mathy stuff for simple programs
module Mathy
( fac
) where


fac :: Integer -> Integer
fac n
    | n < 0 = error "negative number"
    | n == 0 = 1
    | otherwise = n * fac (n - 1)
