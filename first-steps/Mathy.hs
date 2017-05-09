-- Mathy stuff for simple programs
module Mathy
( fib
) where


fib :: Integer -> Integer
fib n
    | n < 0 = error "negative number"
    | n == 0 = 1
    | otherwise = n * fib (n - 1)
