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
    where (word, remainder) = getNextWord y

getNextWord :: String -> (String, String)
getNextWord [] = ([], [])
getNextWord (x:[])
    | isAlphaNum x = ([x], [])
    | otherwise = ([], [])
getNextWord (x:(xs@(y:_)))
    | isAlphaNum x && isAlphaNum y = ((x:word), xs')
    | isAlphaNum x = ([x], xs)
    | otherwise = (word, xs')
    where (word, xs') = getNextWord xs

trimleft :: String -> String
trimleft [] = []
trimleft (x:xs)
    | not (isAlphaNum x) = trimleft xs
    | otherwise = x:xs
