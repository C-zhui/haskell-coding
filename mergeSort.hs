--分解
sequences :: [a] -> [[a]]
sequences [] = []
sequences (x:os) = [x]: (sequences os)

-- 合并两个
merge ::(Ord a) =>(a->a->Bool) -> [a] -> [a]->[a]
merge _ [] ys  = ys
merge  _ xs [] = xs
merge cmp xa@(x:xs) ya@(y:ys) 
    | x `cmp` y = x : merge cmp xs ya 
    | otherwise = y : merge cmp xa ys 

-- 两两 合并
mergeTwoByTwo:: (Ord a) => (a->a->Bool) ->[[a]] ->[[a]]
mergeTwoByTwo _ []  = []
mergeTwoByTwo _ [xs] = [xs]
mergeTwoByTwo cmp (xs:ys:os) = merge cmp xs ys : mergeTwoByTwo cmp os 

-- 全部合并
mergeAll :: (Ord a)=> (a->a->Bool) -> [[a]] -> [a]
mergeAll _ [] = []
mergeAll _ [xs]  = xs
mergeAll cmp xs =  mergeAll cmp ( mergeTwoByTwo cmp xs ) 

-- 归并排序
mergeSort :: (Ord a) => (a->a->Bool)-> [a] ->[a]
mergeSort cmp= mergeAll cmp . sequences
