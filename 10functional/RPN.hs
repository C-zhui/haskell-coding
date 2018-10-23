

-- solveRPN "10 4 3 + 2 * -"
-- solveRPN :: String -> Double
solveRPN s = (words s)

main = do
    line <- getLine 
    print $ solveRPN line