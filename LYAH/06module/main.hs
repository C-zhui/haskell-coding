import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Map as M

arr2 = [1,3..100] ++ [55,49..1] ++ [22..49]

unique = L.nub $ arr2


sentence = "hey these are the words in this sentence"
wordlist = L.words sentence

listWithSomeRepeat = [1,1,1,1,2,2,2,3,3,3,2,2,2,4,4,5,6,7]
afterGroup = L.group listWithSomeRepeat
listSorted = L.sort listWithSomeRepeat 
listGrouped = L.group listSorted

--or ，使用 . 来组合函数 ，由于 . 的优先级比柯里化函数调用低，所以得用 $ 来隔断参数
listGrouped2 = L.group . L.sort $ listWithSomeRepeat



-- 判断是否是子串， contain函数
-- 思路，生成各个长度的尾，用List.tails
isIn ::(Eq a) => [a]->[a]->Bool
isIn xs ys = any (xs `L.isPrefixOf` ) (tails ys)

ifIn = [1,2] `isIn` [-10..10]

tails1 = L.tails [1,2,3,4]

tails :: [a]->[[a]]
tails xs = scanr (:) [] xs

inits :: [a]->[[a]]
inits xs = scanl (\acc x -> acc++[x]) [] xs



encode :: [Char] -> Int -> [Char]
encode s n = map C.chr . map ((+)n) . map C.ord $ s

decode :: [Char] -> Int -> [Char]
-- decode s n = map C.chr . map (+(-n)) . map C.ord $ s
decode s n = encode s (negate n) 





-- 寻找各数之和等于n的数
find :: (a->Bool) -> [a] -> Maybe a
find f [] = Nothing
find f (x:xs) 
    | f x == True = Just x
    | otherwise = find f xs

digitToSum :: Integer -> Integer
digitToSum a =  sum . map (fromIntegral . C.digitToInt) $ show a 

findFirst ::  Integer -> Maybe Integer
findFirst n = find (\x -> digitToSum x == n) [1..]

phoneBook = [
    ("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("wendy","433-2948")
    ,("lacy","443-2978")
    ]

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs)
    | key == k = Just v
    | otherwise = findKey key xs

findKey2 :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey2 key xs = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing xs


mapPhoneBook = M.fromList phoneBook
betty = M.lookup "betty" mapPhoneBook

newPhoneBook = M.insert "grace" "341-9021" mapPhoneBook


string2digit :: String -> [Int]
string2digit =  map C.digitToInt . filter C.isDigit 


intBook = M.map string2digit mapPhoneBook


-- 对重复值的处理，聚合
phoneBookToMap :: (Ord k) => [(k,String)] -> M.Map k String
phoneBookToMap xs = M.fromListWith add xs 
    where add n1 n2 = n1 ++ "," ++ n2


phoneBook2 = [
    ("betty","555-2938")
    ,("betty","555-1234")
    ,("patsy","493-4372")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("wendy","433-2948")
    ,("lacy","443-2978")
    ]


nummap = M.fromListWith max [(1,2),(2,6),(3,9),(4,1),(5,3),(6,4),(7,6),(8,5),(9,8),(10,0),(6,9),(2,8),(1,7),(8,6),(5,5),(3,4)]

main = putStrLn $ show nummap

