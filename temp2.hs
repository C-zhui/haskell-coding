--分解
sequences :: [a] -> [[a]]
sequences [] = []
sequences (x:os) = [x]: (sequences os)

-- 合并两个
merge ::(Ord a) =>[a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xa@(x:xs) ya@(y:ys) 
    | x <= y = x : merge xs ya
    | otherwise = y : merge xa ys

-- 两两 合并
mergeTwoByTwo:: (Ord a) => [[a]] -> [[a]]
mergeTwoByTwo [] = []
mergeTwoByTwo [xs] = [xs]
mergeTwoByTwo (xs:ys:os) = merge xs ys : mergeTwoByTwo os

-- 全部合并
mergeAll :: (Ord a)=> [[a]] -> [a]
mergeAll [] = []
mergeAll [xs] = xs
mergeAll xs =  mergeAll (mergeTwoByTwo xs)

-- 归并排序
mergeSort :: (Ord a) => [a] -> [a]
mergeSort = mergeAll . sequences


