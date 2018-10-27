-- data List a = 
--     Empty | 
--      a `To` (List a)
--     -- Val{listhead::a,listtail::(List a)}  
--     deriving (Show,Read,Eq,Ord)

-- l1 = 1 `To` (2 `To`( 3 `To` Empty))


-- 通常的函数中缀语法是左结合的，即 infixl
infixr 5 :.:
data List a = 
    Empty | 
     a :.: (List a)
    deriving (Show,Read,Eq,Ord)

l2 = 1 :.: 2 :.: 3 :.: Empty

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ xs = xs
(x:.:xs) .++ ys = x:.:(xs.++ys)


data Tree a = 
    EmptyTree |
    Node a (Tree a) (Tree a) 
    deriving (Show)
    
singleton :: (Ord a) => a -> Tree a
singleton a = Node a EmptyTree EmptyTree

treeInsert :: (Ord a)=> a -> Tree a -> Tree a 
treeInsert a EmptyTree = singleton a
treeInsert a (Node v l r) 
    | a == v  = Node a l r
    | a < v = Node v (treeInsert a l) r
    | otherwise = Node v l (treeInsert a r)


treeElem :: (Ord a) => a -> Tree a -> Bool 
treeElem e EmptyTree = False
treeElem e (Node v l r) 
    | e == v = True
    | e < v = treeElem e l
    | otherwise = treeElem e r

nums = [8,6,4,1,7,3,5]
numsTree = foldl (flip treeInsert) EmptyTree nums

