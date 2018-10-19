-- :t 'a'
-- :t 1 

qs :: (Integral a) => [a] -> [a]
qs [] = []
qs (x:xs) = qs (filter (<x) xs) ++ [x] ++ qs (filter (>=x) xs)

-- 显式声明的类型
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [x|x<-st,x `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree a b c = a+b+c

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float 
circumference r = 2 * r * pi

circumference' :: Double -> Double
circumference' r = 2 * r * pi

add :: (Int,Int) -> Int
add t = fst t + snd t

add' :: Int -> Int -> Int
add' a b = a + b

add2 ::(Integral a)=> [a] -> [a]
add2 a = a

ccc :: [a] -> a
ccc a = a !! 0


-- 类型变量，指的是一个字符代表的任意类型的变量，含有类型变量的函数成为多态函数
-- :t head
-- :t fst

-- 类型类，类似接口类型，不能被实例化但是能接受几种实例化的类，可以用作函数参数的类型约束
-- :t (==)
-- -- EQ Ord Show Read Enum Bounded Num Floating Integral 
--  a = compare 1 2
-- :t a

a :: (Integral c) => [c]
a = [1,2,3]


b = fromIntegral (length a) + 3.14

