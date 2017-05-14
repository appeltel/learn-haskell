import Data.Char
import Mathy
import Stringy

main = do
    contents <- getContents
    putStr (show (countTreeExtend (splitWords (map toLower contents)) EmptyTree))
