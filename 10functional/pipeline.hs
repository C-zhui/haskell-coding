infixl 2 >.>
(>.>) :: (a->b)->(b->c)->a->c
f >.> g = g . f

infixr 1 >->
( >->) :: a -> (a->b) -> b
a  >-> f = f a 

main :: IO()
main = do
    let a = [1..10]::[Int]
    a >-> map (*2) >.> sum >.> print
