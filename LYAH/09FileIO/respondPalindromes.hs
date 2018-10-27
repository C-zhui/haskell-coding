isPal :: String -> Bool
isPal s = s == reverse s

respondPalindromes ::String -> String
respondPalindromes = 
    unlines .
    map (\line -> if isPal line then "isPal" else "is not Pal") .
    lines

main = interact respondPalindromes

