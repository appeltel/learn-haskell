import qualified Mathy as M

main = do
    putStrLn "Enter a positive integer:"
    number <- getLine
    putStrLn (number ++ "! = " ++ (show (M.fac (read number::Integer))))
