factorial 0 = 1
factorial n = n * factorial (n-1)

ins :: (Ord a)=>a->[a]->[a]
ins a [] = [a]
ins a (x:xs) 
 | a<=x = a:x:xs
 | otherwise = x : ins a xs

iSort ::(Ord a)=>[a]->[a]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

iSort' ::(Ord a)=>[a]->[a]
iSort' xs = iSort_ xs []
 where 
  iSort_ ::(Ord a)=>[a]->[a]->[a]
  iSort_ [] xs = xs
  iSort_  (x:xs) sortedls = iSort_ xs (ins x sortedls)

iSort''::(Ord a)=>[a]->[a]
iSort'' xs = foldl (flip ins) [] xs

swaps ::(Ord a)=>[a]->[a]
swaps (x:y:xs) 
 | x < y = x:(swaps (y:xs))
 | otherwise = y:(swaps (x:xs))
swaps a = a


fix ::(Eq a)=>(a->a)->a->a
fix f x = let x' = f x in if x' == x then x else fix f x'
 

bubbleSort ::(Ord a,Eq a)=>[a]->[a]
bubbleSort = fix swaps

bubbleSort' ::(Ord a,Eq a)=>[a]->[a]
bubbleSort' [] = []
bubbleSort' xs = 
  let afterSwaps = swaps xs;
      lastelem = last afterSwaps;
      initLs = init afterSwaps
   in bubbleSort' initLs ++ [lastelem]

delete ::(Eq a)=>a->[a]->[a]
delete _ [] = []
delete a (x:xs) 
 | a == x = xs
 | otherwise = x:delete a xs

selectSort ::(Ord a)=>[a]->[a]
selectSort [] = [] 
selectSort xs = 
 let minone = minimum xs
  in minone:(selectSort (delete minone xs))

quickSort ::(Ord a)=>[a]->[a]
quickSort (x:y:xs) = quickSort (filter (<x) (y:xs)) ++ [x] ++ quickSort (filter (>=x) (y:xs))
quickSort a = a


filterSplit ::(a->Bool)->[a]->([a],[a])
filterSplit f [] = ([],[])
filterSplit f (x:xs) 
 | f x = (x:l,r)
 | otherwise = (l,x:r)
 where (l,r) = filterSplit f xs

qs ::(Ord a)=>[a]->[a]
qs (x:y:xs) = qs l ++ [x] ++ qs r
 where (l,r) = filterSplit (<x) (y:xs)
qs a = a

sequences :: [a]->[[a]]
sequences = map (\x -> [x]) 

merge ::(Ord a)=>[a]->[a]->[a]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)  
 | x < y = x:merge xs (y:ys)
 | otherwise = y:merge (x:xs) ys


mergeTwoByTwo :: (Ord a)=>[[a]]->[[a]]
mergeTwoByTwo (xs:ys:xss) = 
 merge xs ys :mergeTwoByTwo xss
mergeTwoByTwo a = a

mergeAll ::(Ord a)=>[[a]]->[a]
mergeAll [xs] = xs
mergeAll allElem@(xs:ys:xss) = mergeAll (mergeTwoByTwo allElem)

mergeSort ::(Ord a)=>[a]->[a]
mergeSort  = mergeAll . sequences
