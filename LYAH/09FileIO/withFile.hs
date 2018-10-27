import System.IO

main = do
    withFile "haiku.txt" ReadMode (\handle -> do
        content <- hGetContents handle
        putStr content)
    
