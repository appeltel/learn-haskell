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

ffib :: Integer -> Integer
ffib = getIndex (map f (map toInteger [0 ..]))
    where f 1 = 1
          f 2 = 2
          f n = ffib (n-2) + ffib (n-1)

--memoize :: (Integer -> Integer) -> (Integer -> Integer)
--memoize f = getIndex (map f (map toInteger [0 ..]))

--fix :: (a -> a) -> a
--fix f = let {x = f x} in x

--ffib :: Integer -> Integer
--ffib = fix (memoize . fib)
