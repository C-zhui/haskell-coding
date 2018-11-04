{-
    huffman tree 
    1. 对所有的码(包装在HFM_node里面)按照出现频率排序为一个链表 <
    2. 每次取最小频率的两个，合成一个节点，重新返回链表，插入正确的位置
    3. 重复2 直到只剩下 一个节点，即完成编码
-- -}
import Data.List
import Data.Ord

data HFM_node = 
    Leaf{
        tag ::String,
        weight::Double
    }
    |Branch {
        weight::Double,
        left ::HFM_node,
        right ::HFM_node
    }   deriving(Show)

-- 建树
hTree :: [HFM_node]->HFM_node
hTree (n:[]) = n
hTree (x:y:xs) = hTree (insertBy (comparing weight) (Branch (weight x + weight y) x y) xs)
hTree [] = error "Non solution"

-- 序列化 
serialize :: HFM_node->[(Double,String,String)]
serialize (Leaf t w) = [(w,t,"")]
serialize (Branch _ l r) = [(w,t,'0':code)|(w,t,code)<-serialize l] ++ [(w,t,'1':code)|(w,t,code)<-serialize r]

-- 输入转换HFM_node
lineToNode::String -> HFM_node
lineToNode = (\ (x:y:_) -> Leaf x (read y::Double)) . words

readDataToNode :: String -> [HFM_node]
readDataToNode = map  lineToNode . lines

-- 转换树为编码
huffman :: [HFM_node]->[(Double,String,String)]
huffman  =  sortBy (comparing (negate . (\(x,_,_)->x))) . serialize . hTree . (sortBy (comparing weight)) 

-- 计算带权长度
getAvgLen :: [(Double,String,String)]->Double
getAvgLen = sum . map (\(w,_,code)-> w * fromIntegral (length code))

-- 计算信息熵
getInfoEntropy :: [(Double,String,String)]->Double
getInfoEntropy = sum . map (\(w,_,_) -> negate $ w * (logBase 2 w))

main :: IO()
main = do
    s <- readFile "data.txt"    
    let nodeArr = readDataToNode s
        hfm = huffman nodeArr
        avgLen = getAvgLen hfm
        infoEntropy = getInfoEntropy hfm
        redundence = 1 - infoEntropy / avgLen
    print nodeArr
    putStrLn " "
    print hfm
    putStrLn " "
    putStr "the weighted average length is :"
    print   avgLen
    putStrLn " "
    putStr "the  Informationentropy is : "
    print  infoEntropy
    putStrLn " "
    putStr "the  redundence is : "
    print $ redundence
    print $ sum . map (weight) $ nodeArr


