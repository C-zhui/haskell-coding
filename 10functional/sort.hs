iSort::(Ord a)=>[a] -> [a]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: (Ord a) => a -> [a] -> [a]
ins a [] = [a]
ins a (x:xs) 
 | a <= x = a:x:xs
 | otherwise = x:(ins a xs)