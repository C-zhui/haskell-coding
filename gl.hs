main = do
    line <- fmap reverse getLine
    putStr $ line
   