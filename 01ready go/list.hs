arr = [1,6,2,4]

arr2 = arr ++ [13,42]

arr3 = 3:arr


qs :: (Integral a) => [a] -> [a]
qs [] = []
qs (x:xs) = qs (filter (<x) xs) ++ [x] ++ qs (filter (>=x) xs)

h_arr = head arr

t_arr = tail arr 

i_arr = init arr 

l_arr = last arr 

-- drop 2 arr

-- length arr

-- maximum arr 

-- minimun arr

-- take 3 [1..]

-- null [1..]

-- sum arr

-- product arr

-- 1 elem arr

arr4 = [1..10]

-- [1,3..100]


-- [20,19..1]

-- ['a'..'z']

-- ['\0'..'\127']

-- take 100 [1,3..]

arr5 = take 10 (cycle [1,2,3])

-- take 3 (repeat 5)

-- replicate 3 5


-- [0.1 ,0.2 .. 1]

