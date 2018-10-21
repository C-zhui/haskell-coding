
q = Just "Good"

fromMaybe :: Maybe a -> a
fromMaybe (Just a) = a
fromMaybe Nothing = error "nothing in here"


flowInto :: a -> (a->c) -> c
a `flowInto` f = f a

pipe :: (a->b) -> (b->c) -> a -> c
f `pipe` g = g . f



data Vector a = Vector a a a deriving(Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a 
vplus (Vector a b c) (Vector i j k) = Vector (a+i) (b+j) (c+k)

dotProd :: (Num a) => Vector a -> Vector a -> a
dotProd (Vector a b c) (Vector i j k) = sum $ zipWith (*) [a,b,c] [i,j,k]

vmult :: (Num a) => Vector a -> Vector a -> Vector a 
vmult (Vector a b c) (Vector i j k) = Vector (a*i) (b*j) (c*k)


enumerate :: [a] -> [(Int,a)]
enumerate [] = []
enumerate xs = zip [0..] xs

enumStr =  enumerate "hello world"

