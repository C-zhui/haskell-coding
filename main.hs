f :: (Integral a) => a-> a
f 0 = 1
f n = n * (f (n-1))
main = putStrLn (show (f 10))

qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (x:xs) = qs (filter (<x) xs) ++ [x] ++ qs (filter (>=x) xs)

empty :: [a] -> Bool
empty (x:a) = False
empty _ = True

square :: Int -> Int
square x = x * x

