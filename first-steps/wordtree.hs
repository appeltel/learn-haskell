import Data.Char
import Mathy
import Stringy

main = do
    contents <- getContents
    putStr (show (treeExtend (splitWords (map toLower contents)) EmptyTree))
