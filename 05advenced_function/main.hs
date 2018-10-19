
qs :: (Ord a) => [a] -> [a]
qs al@(x:_:_) = qs (filter (<x) al) ++ (filter(==x) al ) ++ qs (filter(>x) al )
qs a = a


-- max 4 5 

-- (max 4) 5

a = max 4

b = a 5

multipleThree :: (Num a) => a -> a -> a -> a 
multipleThree a b c = product [a,b,c]

a1 = multipleThree 1
b1 = a1 2 
c1 = b1 3
-- c1 == 6


compareWithHundred :: Int -> Ordering
compareWithHundred a = compare 100 a

compareWithHundred' = compare 100

arr = [1,2,3]

-- 违反直觉的 不等式，这种行为称为截断，把一个中缀表达式放在括号中，参数为中缀表达式缺少的一侧
d = (>) 1
arr2 = filter (>1) arr -- 截断
arr3 = filter d arr

divideByTen :: (Floating a)=> a -> a
divideByTen = (/10)
o = divideByTen 120

isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A'..'Z'])


doubleMe x = x+x

-- 使用函数作为参数，并对参数值应用两次
applyTwice :: (a->a) -> a -> a
applyTwice f x = f (f x)

s = applyTwice doubleMe 3
w = applyTwice (++[123]) [456]


zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs


z = let f x y = (x,y) in zipWith' f [1,2,3] "asd"
z1 = let f x y = (x,y) in zipWith' f [1..] ['a'..'z']

-- 交换两个参数的位置
flip' :: (a->b->c) -> (b->a->c)
flip' f = g     -- 用g 来包装对f的调用
    where g x y = f y x

-- 更简洁的写法,函数实现上，左边定义了接收参数的顺序，右边定义了调用参数的顺序
filp'' :: (a->b->c )->b->a->c
filp'' f x y = f y x

map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a->Bool) -> [a]->[a]
filter' _ [] = []
filter' f (x:xs) = if f x   then x: filter' f xs 
                            else filter' f xs

-- $ 操作符左边是一个函数，右边是一个参数，可以使用右边的结果作为参数而不用加 ()
g = sum $ takeWhile (<10000) $ filter odd $ map (^2) [1..]


klzChain :: Integer -> [Integer]
klzChain 1 = [1]
klzChain n 
    | even n = n:klzChain(n `div` 2)
    | odd n = n:klzChain(n * 3 + 1)

-- 算出 1..100 开始的klzChain的长度大于15的有多少个
chains = length $ filter (\c -> c > 15 ) $map (\c -> length c ) $ map klzChain [1..100]


-- 多参数映射，映射一次，将返回一个函数
listOfFunc = map (*) [0..]
res = listOfFunc !! 4 $ 5


-- lambda函数，匿名函数,无法设置匹配
mulThem = zipWith (\x y -> x*y) [1,3..11] [2,4..]


foldl' :: (a->b->a) -> a -> [b] -> a
foldl' f acc [] = acc
foldl' f acc ls@(x:xs) = foldl' f acc' xs
  where acc' = f acc x

foldr' :: (b->a->a) -> a -> [b] -> a
foldr' f acc [] = acc
foldr' f acc (x:xs) = f x acc'
  where acc' = foldr' f acc xs


scanl' :: (a->x->a) -> a ->[x] -> [a]
scanl' f acc [] = [acc]
scanl' f acc (x:xs) =
  acc' : scanl' f acc' xs
  where acc' = f acc x

scanr' :: (x->a->a) -> a -> [x] -> [a]
scanr' f acc [] = [acc]
scanr' f acc (x:xs) = 
  accnew : accs
  where accs = scanr' f acc xs
        accnew = f x (accs !! 0)

-- scanl (+) 0 [1..10]
-- scanl (flip (:)) [] [1..10]
aaa = takeWhile (<1000) $scanl (+) 0 ( map sqrt [1..] )




-- $ 函数应用符，以中缀表达式来接收参数，左边是一个函数f，右边是一个任意的参数x，使用f来应用x，即 f x，用于将函数与参数分开，减少()的使用
-- map ($ 3) [(4+),(10*),(^2),sqrt]


-- . 函数组合，用来组合函数
vb = map (\x -> negate (abs x)) [-4,2,6,-3]
vb2 = map (negate . abs . (*2)) [-4,2,6,-3]
vb3 = map (negate . sum . tail) [ [1..5] , [3..6] , [1..7] ]

pmzm = product . map (*3) $ zipWith max [1,2] [4,5]

-- point-free 
sss xs = foldl (+) 0 xs
-- equivalent
sss' ::(Num a) => [a] -> a
sss' = foldl (+) 0 
 
flipv:: (a->b->c)->b->a->c
flipv f = \x y -> f y x

