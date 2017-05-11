-- Mathy stuff for simple programs
module Mathy
( fac,
  fib,
  ffib
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

getIndex :: [Integer] -> Integer -> Integer
getIndex (x:xs) 0 = x
getIndex (x:xs) n = getIndex xs (n - 1)

memoize :: (Integer -> Integer) -> (Integer -> Integer)
memoize f = getIndex (map f (map toInteger [0 ..]))

fix :: (a -> a) -> a
fix f = let x = f x in x

cfib :: (Integer -> Integer) -> Integer -> Integer
cfib f 1 = 1
cfib f 2 = 1
cfib f n = f (n-1) + f (n-2)

ffib :: Integer -> Integer
ffib = fix (memoize . cfib)
