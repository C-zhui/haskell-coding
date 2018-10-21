import Data.List as L

f :: (Integral a) => a-> a
f 0 = 1
f n = n * (f (n-1))

qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (x:xs) = qs (filter (<x) xs) ++ [x] ++ qs (filter (>=x) xs)

empty :: [a] -> Bool
empty (x:a) = False
empty _ = True

square :: Int -> Int
square x = x * x

primes::[Integer]
primes = getPrimes [2..]
    where getPrimes (p:xs) = p: getPrimes [x|x<-xs ,x `mod` p/=0]

a = take 10000 primes

main = do
    print $ a !! 99


 