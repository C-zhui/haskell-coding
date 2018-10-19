lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven!"
lucky x = "Sorry,you're out of luck,pal!"

sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"

factorial :: (Integral n) => n -> n
factorial 0 = 1
factorial n = n * factorial (n-1)


charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"

-- 向量加
addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
-- addvec a b = (fst a + fst b,snd a + snd b)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a  
first (x, _, _) = x  

second :: (a, b, c) -> b  
second (_, y, _) = y  

third :: (a, b, c) -> c  
third (_, _, z) = z

myHead :: [a] -> a
myHead [] = error "try peek fst elem from a empty list"
myHead (x:_) = x

mtail :: [a] -> [a]
mtail [] = error "can not apply tail to a empty list"
mtail (_:xs) = xs

mlast :: [a] -> a
mlast [] = error "can not apply last to a empty list"
mlast (x:xs) = if null xs then x else mlast xs

minit :: [a] -> [a]
minit [] = error "can not apply init to a empty list"
minit (x:xs) = if null xs then [] else x:(minit xs)


badAdd :: (Num a)=> [a] -> a
badAdd (z:x:c:[]) = z+x+c

-- As ，可以保留整体的一个引用
qs :: (Ord a) => [a] -> [a]
qs al@(x:_:_) = qs (filter (<x) al) ++ (filter (==x) al) ++ qs(filter (>x) al)
qs a = a

-- 使用分号可以将函参匹配写成一行
qs2 :: (Ord a)=> [a] -> [a] ; qs2 [] = [] ;qs2 (x:xs) = qs2 (filter (<x) xs ) ++ [x] ++ qs2 (filter (>=x) xs)


-- Guard，便于多条件语句的书写
bmiTell :: Double -> String
bmiTell bmi 
    | bmi <= 18.5 = "You are thin"
    | bmi <= 25.0 = "You are normal"
    | bmi <= 30.0 = "You are fat"
    | otherwise = "You are a whale"


bmiTell2 :: Double -> Double -> String
bmiTell2 weight height
    | weight/height^2 <= 18.5 = "You are thin"
    | weight/height^2<= 25.0 = "You are normal"
    | weight/height^2<= 30.0 = "You are fat"
    | otherwise = "You are a whale"



max' :: (Ord a) => a -> a -> a
max' a b 
    | a < b = b
    |otherwise = a 

myCompare :: (Ord a)=> a ->a ->Ordering
myCompare a b 
    | a == b = EQ
    | a < b = LT
    | otherwise = GT

-- where 表达式，用于定义局部的变量，对于多模式匹配的函数，建议是把参数放在全局
bmiTell3 :: Double -> Double -> String
bmiTell3 weight height
    | bmi <= skinny = "You are thin"
    | bmi <= normal = "You are normal"
    | bmi <= fat = "You are fat"
    | otherwise = "You are a whale"
    where   bmi = weight / ( height ^ 2 ) 
            skinny = 18.5
            normal = 25.0
            fat = 30.0

-- where 中的模式匹配
initial :: String -> String -> String
initial firstname lastname = "(" ++ [f] ++ "." ++ [l] ++")"
    where   (f:_) = firstname
            (l:_) = lastname

-- where 中的函数
calcBmis :: [(Double,Double)] -> [Double]
calcBmis whs = [bmi w h| (w,h) <- whs]
    where bmi weight height = weight/height^2



-- let，用于在表达式求值之前计算中间变量
cylinder :: Double->Double->Double
cylinder r h = 
    let 
        sideArea = product [2,r,h]
        topArea = product [r,r,pi]
    in sum [sideArea , topArea , topArea]

-- 4 * (let a = 9 in a + 1) + 2

--  let square x = x ^ 2 in [square x | x <- [1..10]]

-- (let a = 100;b = 200 ;c =300 in product [a,b,c],let foo="hey";bar = "there!" in foo ++ bar)

-- (let (a,b,c) = (1,2,3) in product [a,b,c])

-- -- let 用于表达式 ， where用于单一匹配函数，在哨卫中也有效

calcBmis2 :: [(Double,Double)] -> [Double]
calcBmis2 whs = [bmi | (w,h) <- whs,let bmi = w/h^2]



-- case, 函数模式匹配的原型
head' :: [a] -> a
head' xs = case xs of   [] -> error "can not apply this to a empty list"
                        (x:_) -> x

describeList :: [a] -> String
describeList xs = case xs of    [] -> "a empty list"
                                [x] -> "Singleton list"
                                a -> "a long list"

describeList2 :: [a] -> String
describeList2 ls = "this list is a " ++ what ls
    where   what [] = "empty list"
            what [x] = "singleton list"
            what a = "longer list"

