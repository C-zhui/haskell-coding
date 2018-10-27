{-
    huffman tree 
    1. 对所有的码(包装在HFM_node里面)按照出现频率排序为一个链表 <
    2. 每次取最小频率的两个，合成一个节点，重新返回链表，插入正确的位置
    3. 重复2 直到只剩下 一个节点，即完成编码
-}

data HFM_node = 
    HFM_None_node | 
    HFM_node {
        tag ::String,
        weight::Double,
        left_node ::HFM_node,
        right_node ::HFM_node
    }   deriving(Show)

instance Eq HFM_node where 
    a == b = weight a == weight b

instance Ord HFM_node where 
    a <= b = weight a <= weight b

-- 插入
ins:: (Ord a) => a -> [a]->(a->a->Bool) ->[a]
ins a [] _ = [a]
ins a xAll@(x:xs) cmp
    | a `cmp` x == True = a:xAll
    | otherwise = x:ins a xs cmp

-- 插入排序
insertSort :: (Ord a)=>[a] -> [a]
insertSort a = foldl (\acc x -> ins x acc (<=)) [] a

lineToNode::String -> HFM_node
lineToNode = (\ (x:y:_) -> HFM_node x (read y::Double)  HFM_None_node  HFM_None_node) . words

readDataToNode :: String -> [HFM_node]
readDataToNode = map  lineToNode . lines

printArrLineByLine ::(Show a)=> [a] -> IO()
printArrLineByLine [] = return ()
printArrLineByLine (x:xs) = do
    print x
    printArrLineByLine xs

reduceToOne :: [HFM_node]-> HFM_node
reduceToOne [] = error "at least give me one!"
reduceToOne [one] = one
reduceToOne (x:y:xs) = 
    let newnode = HFM_node "" (weight x + weight y) x y 
        in reduceToOne (ins newnode xs (<))

main :: IO()
main = do
    s <- readFile "data.txt"    
    let 
        nodeArr = readDataToNode s
        tree = reduceToOne ( insertSort nodeArr )
    printPath tree
    putStr "the average weighted length is :"
    print $  getAverageLen tree
    
printPath::HFM_node->IO()
printPath n = do
    printNode n 0
    where 
        printNode HFM_None_node _ = return ()
        printNode n d  
            | tag n /= "" = do -- 叶子
                putStrLn (" : " ++ tag n)
            | otherwise = do -- 分支
                printLR n (d)

        printLR n d = do
            putStr "0 "
            printNode (left_node n ) (d+2)
            putStr (replicate d ' ')
            putStr "1 "
            printNode (right_node n) (d+2)

getAverageLen :: HFM_node -> Double
getAverageLen HFM_None_node = 0
getAverageLen n = getavg n 0
    where 
        getavg :: HFM_node -> Int ->Double
        getavg n d 
            | tag n /= "" = (weight n )*(fromIntegral d)
            | otherwise = 
                let leftres = getavg (left_node n) (d+1);
                       rightres = getavg (right_node n) (d+1)
                in leftres +  rightres 