main = do
    putStrLn "Hi, what is your name?"
    name <- getLine
    putStrLn ("Hello " ++ name ++ ", nice to meet you.")
