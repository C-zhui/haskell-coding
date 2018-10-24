-- solveRPN "10 4 3 + 2 * -"
solveRPN :: String -> Double 
solveRPN = head . solveFunction . words

solveFunction :: [String] -> [Double]
solveFunction ss = foldl f [] ss

 where 
       f :: [Double] -> String -> [Double]
       f (x:y:xs) "+" = (y+x): xs
       f (x:y:xs) "-" = (y-x): xs
       f (x:y:xs) "*" = (y*x):xs
       f (x:y:xs) "/" = (y/x):xs
       f (x:y:xs) "^" = (y**x):xs
       f xs "sum" = [sum xs]
       f xs num = read num:xs