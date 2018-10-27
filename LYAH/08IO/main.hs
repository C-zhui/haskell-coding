import qualified Data.Char as C
-- main :: IO()
-- main = do
--     putStrLn "Hello, what's your name?"
--     name <- getLine
--     putStrLn $  "Good day " ++ name




-- main :: IO()
-- main = do
--     putStrLn "What 's your first name?"
--     firstName <- getLine
--     putStrLn "What's your last name ?"
--     lastName <- getLine
--     let
--         bigFirstName = map C.toUpper firstName
--         bigLastName =  map C.toUpper lastName 
--     putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"


main::IO()
main = do
    line <- getLine
    if null line 
        then return ()
        else do 
            line `flowInto` (reverseWords `pipe` putStrLn)
            main


flowInto :: a ->(a -> b) -> b
a `flowInto` f = f a 

pipe :: (a->b) -> (b->c) -> a->c
f `pipe` g = g . f

reverseWords :: String -> String
reverseWords = words `pipe` (map reverse) `pipe` unwords

