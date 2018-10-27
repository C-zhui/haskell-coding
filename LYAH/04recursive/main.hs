fibonacii :: Int -> Int 
fibonacii n = case n of 0 -> 0
                        1 -> 1
                        a -> fibonacii (n-1) + fibonacii (n-2)

main = putStrLn (show (fibonacii (32)))

maximum' :: (Ord a)=> [a] -> a
maximum' [] = error "this is a empty list"
maximum' [x] = x
maximum' (x:rs) = max x (maximum' rs)



replicate' :: (Num i,Ord i) => i -> a -> [a]
replicate' n a 
    | n <= 0 = []
    | otherwise = a:(replicate' (n-1) a)

take' :: Int -> [a] -> [a]
take' n a 
    | n <= 0 = []
    | otherwise = case a of [] -> []
                            (x:xs) -> x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a->[a]
repeat' a = a:repeat' a


zip' :: [a]->[b]->[(a,b)]
zip'  _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

elem' ::(Eq e) => e -> [e] -> Bool
elem' a [] = False
elem' a (x:xs) 
    | a == x        = True
    | otherwise     = a `elem'` xs

qs :: (Ord a) => [a] -> [a]
qs al@(x:_:_) = qs (filter (<x) al) ++ (filter(==x) al ) ++ qs (filter(>x) al )
qs a = a