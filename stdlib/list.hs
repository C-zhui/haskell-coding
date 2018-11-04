{-
    (++),  head,  last,  tail,  init,  null,  length,  map,  reverse,  
    intersperse,  intercalate,  transpose,  subsequences,  permutations,  
    foldl,  foldl',  foldl1,  foldl1',  foldr,  foldr1,  concat,  concatMap,  
    and,  or,  any,  all,  sum,  product,  maximum,  minimum,  scanl,  scanl1,  
    scanr,  scanr1,  mapAccumL,  mapAccumR,  iterate,  repeat,  replicate,  
    cycle,  unfoldr,  take,  drop,  splitAt,  takeWhile,  dropWhile,  span,  
    break,  stripPrefix,  group,  inits,  tails,  isPrefixOf,  isSuffixOf,  
    isInfixOf,  elem,  notElem,  lookup,  find,  filter,  partition,  (!!),  
    elemIndex,  elemIndices,  findIndex,  findIndices,  zip,  zip3,  zip4,  
    zip5,  zip6,  zip7,  zipWith,  zipWith3,  zipWith4,  zipWith5,  zipWith6,  
    zipWith7,  unzip,  unzip3,  unzip4,  unzip5,  unzip6,  unzip7,  lines,  
    words,  unlines,  unwords,  nub,  delete,  (\\),  union,  intersect,  sort,  
    insert,  nubBy,  deleteBy,  deleteFirstsBy,  unionBy,  intersectBy,  
    groupBy,  sortBy,  insertBy,  maximumBy,  minimumBy,  genericLength,  
    genericTake,  genericDrop,  genericSplitAt,  genericIndex,  genericReplicate  

-- -}
import Data.List

catTwo :: [a]->[a]->[a]
catTwo a b = foldr (:) b a

head' :: [a] -> a
head' [] = undefined
head' (x:xs) = x

last' :: [a]->a
last' [] = undefined
last' a = foldr1 (const id) a

tail' :: [a]->[a]
tail' [] = undefined
tail' (x:xs) = xs

init' :: [a]->[a]
init' [] = undefined
init' xs =  reverse . tail . reverse $ xs

null' ::[a]->Bool
null' [] = True
null' (x:xs) = False

length' ::[a]->Int
length' xs = sum . map (\x->1) $ xs

map' ::(a->b)->[a]->[b]
map' f [] = [] 
map' f (x:xs) = f x :map' f xs

reverse' ::[a] -> [a]
reverse' xs = foldl (flip (:)) [] xs

intersperse' :: a->[a]->[a]
intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' i (x:xs) = i:x:intersperse' i xs

intercalate' :: [a]->[[a]]->[a]
intercalate' xs xss = concat (intersperse' xs xss)

transpose' :: [[a]] -> [[a]]
transpose' xss = trans $ fnn  xss
    where trans [] = []
          trans xss = (map head xss) : (trans $ fnn . map tail $ xss)
          fnn = filter (not . null)

subsequences' :: [a]->[[a]]
subsequences' xs = [] ++ nonEmptySubsequences' xs

nonEmptySubsequences' :: [a] -> [[a]]
nonEmptySubsequences' []      =  []
nonEmptySubsequences' (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences' xs)
  where f ys r = ys : (x : ys) : r

-- permutations :: [a]->[[a]]
--   permutations

mfoldl :: (a->x->a)->a->[x]->a
mfoldl f a [] = a
mfoldl f a (x:xs) = foldl f (f a x) xs

-- mfoldl' :: (a->x->a)->a->[x]->a
-- mfoldl' f a [] = a
-- mfoldl' f a (x:xs) = foldl f a' xs
--     where a' = seq a (f a x)

mfoldl1 :: (a -> a -> a) -> [a] -> a
mfoldl1 f (x:xs) = mfoldl f x xs
mfoldl1 _ [] = error "empty list"

mfoldr :: (x->a->a)->a->[x]->a
mfoldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys

mfoldr1 ::(a->a->a)->[a]->a
mfoldr1 f = go
    where 
        go [x] = x
        go (x:xs) = f x (go xs)
        go [] = error "empty list"

concat' ::[[a]] -> [a]
concat' = foldr (++) []

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f xs = concat . map f $ xs

and
--  and,  or,  any,  all,  sum,  product,  maximum,  minimum,  scanl,  scanl1, 