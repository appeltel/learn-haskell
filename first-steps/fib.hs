import qualified Mathy as M

main = do
    putStrLn "Enter a positive integer:"
    number <- getLine
    putStrLn ("Fibonacci number " ++ number ++ " is " ++ (show (M.ffib (read number::Integer))))
