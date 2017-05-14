-- Mathy stuff for simple programs
module Mathy
( fac,
  fib,
  ffib,
  singleton,
  treeInsert,
  treeElem,
  treeExtend,
  countTreeInsert,
  countTreeElem,
  countTreeExtend,
  Tree(EmptyTree, Node),
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

-- Trees are kinda mathy, to be honest (taken from learnyouahaskell)
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

treeExtend :: (Ord a) => [a] -> Tree a -> Tree a
treeExtend [] t = t
treeExtend (x:xs) t = treeExtend xs (treeInsert x t)

countTreeInsert :: (Ord a) => a -> Tree (a, Int) -> Tree (a, Int)
countTreeInsert x EmptyTree = singleton (x, 1)
countTreeInsert x (Node (a, n) left right)
    | x == a = Node (a, n+1) left right
    | x < a = Node (a, n) (countTreeInsert x left) right
    | x > a = Node (a, n) left (countTreeInsert x right)

countTreeElem :: (Ord a) => a -> Tree (a, Int) -> Int
countTreeElem x EmptyTree = 0
countTreeElem x (Node (a, n) left right)
    | x == a = n
    | x < a = countTreeElem x left
    | x > a = countTreeElem x right
countTreeExtend :: (Ord a) => [a] -> Tree (a, Int) -> Tree (a, Int)
countTreeExtend [] t = t
countTreeExtend (x:xs) t = countTreeExtend xs (countTreeInsert x t)
