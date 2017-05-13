-- Stringy stuff for simple programs
module Stringy
( splitWords,
  getNextWord,
  trimleft
) where

import Data.Char

splitWords :: String -> [String]
splitWords x = fst (aSplitWords ([], x))

aSplitWords :: ([String], String) -> ([String], String)
aSplitWords (x, []) = (x, [])
aSplitWords (x, y)
    | (word, remainder) == ([], []) = (x, y)
    | remainder == [] = (x++[word], [])
    | otherwise = aSplitWords (x++[word], remainder)
    where (word, remainder) = getNextWord ([], y)

getNextWord :: (String, String) -> (String, String)
getNextWord (x, []) = (x, [])
getNextWord ([], (x:xs))
    | not (isAlphaNum x) = getNextWord ([], xs)
    | otherwise = getNextWord ([x], xs)
getNextWord (a, (x:xs))
    | not (isAlphaNum x) = (a, (x:xs))
    | otherwise = getNextWord ((a ++ [x]), xs)

trimleft :: String -> String
trimleft [] = []
trimleft (x:xs)
    | not (isAlphaNum x) = trimleft xs
    | otherwise = x:xs
