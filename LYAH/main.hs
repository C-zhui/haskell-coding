foldl' f acc [] = acc
foldl' f acc (x:xs) = 
    let acc' = f acc $! x
        in foldl' f acc' xs

-- a :: Int
a = foldl' (+) (0::Int) ([1..10000000]::[Int])