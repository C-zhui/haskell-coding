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


gcd' n m = if m==0 then n else gcd' m (n `mod` m)

power _ 0 = 1
power x n = x * power x (n-1)

power' x 0 = 1
power' x n 
    | odd n = let p = power x ((n-1) `div` 2) in x*p*p
    | otherwise =  let p = power x (div n 2) in p*p

delete :: (Eq a)=>a -> [a]->[a]
delete _ [] = []
delete a (x:xs) 
    | a == x = delete a xs
    | otherwise = x:delete a xs


-- 扩展递归
total :: (Num a )=>[a] ->a
total [] = 0
total (x:xs) = x + total xs

-- 尾递归
total' ::(Num a)=>[a]->a
total' [] = 0
total' xs = 
    tt xs 0 
    where 
        tt  [] acc= acc
        tt  (x:xs) acc = tt  xs (acc + x)

factorial::Int->Int
factorial 0 = 1
factorial n = factorial (n-1) * n

!x = factorial 1000

mc n 
    | n>100 =n-10
    |otherwise = mc (mc (n+11))




fibStep (u,v) = (v,u+v)

fibPair 0 = (0,1)
fibPair n = fibStep (fibPair $! (n-1))


iterate' :: (a->a)->a->[a]
iterate' f a  = let ao = f a in ao : iterate' f ao
