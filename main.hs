import Data.List as L
import Data.Char as C
break' :: (a->Bool)->[a]->([a],[a])
break' f xss = 
    let (xs,ys) = b f [] xss
        in (reverse xs,ys)
    where 
        b :: (a->Bool)->[a]->[a]->([a],[a])
        b _ xs [] = (xs,[])
        b bf xs (y:ys) = if bf y then (xs,y:ys) else b bf (y:xs) ys

splitLines :: String -> [String]
splitLines [] = []
splitLines ss = 
    let (xs,ys) = break' isLineTerminate ss
        in xs : case ys of 
            ('\r':'\n':rest) -> splitLines rest
            ('\r':rest)      -> splitLines rest
            ('\n':rest)      -> splitLines rest
            _                -> []
    where 
        isLineTerminate c = c=='\r' || c=='\n'

splitWords :: String -> [String]
splitWords [] = []
splitWords ss
    | isWhitespace (head ss) = splitWords . tail $ ss
    | otherwise = 
        let (xs,ys) = break' isWhitespace ss
        in xs : splitWords ys
    where 
        isWhitespace c = c==' ' || c=='\t' || c=='\n' || c=='\r'


-- (|>) ::a->(a->c)->c 
(|>) = flip ($)

a = 1.0 |> (*2)


arr::Int
arr = 
    let a = L.foldl' (+) 0 [1..200000000]
        in seq a (a+1)

append :: [a] -> [a] -> [a]
append x y = foldr (:) y x

concat' :: [[a]] -> [a]
concat' [] = []
concat' xss = foldr (++) [] xss

main = do
    line <- getLine 
    print . map C.ord $ line
