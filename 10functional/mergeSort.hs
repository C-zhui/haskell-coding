merge :: (Ord a) => [a] -> [a] -> [a]
merge a [] = a
merge [] a = a
merge aall@(a:as) ball@(b:bs) 
 | a <= b = a : (merge as ball)
 | otherwise = b : (merge aall bs)

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [x] = [x]
mergeSort xs = 
  merge (mergeSort (take half xs)) (mergeSort (drop half xs)) 
  where half = length xs `div` 2 

main :: IO()
main = do
 let a = mergeSort $ [1..1000]++[2000,1999..1]++[2,4..3000]
 print a 
