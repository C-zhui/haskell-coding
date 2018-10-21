import Control.Monad

-- main :: IO()
-- main = do
--     let 
--         arr = [1,2,3,4]
--         strs =["hi","how","are","you"] 
--     print arr
--     print $ map (++"!") strs

main :: IO()
main = do
    input <- getLine
    when (input == "SWORDFISH") $ do
        putStrLn input

