data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True 
    Green == Green = True
    Yellow == Yellow =True
    _ == _ = False 

instance Show TrafficLight where 
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"


data Maybe' a = Nothing' | Just' a

instance (Eq m)=>Eq (Maybe' m) where 
    Just' a == Just' b = a == b
    Nothing' == Nothing' = True
    _ == _  = False
 
instance (Show m)=>Show (Maybe' m) where 
    show Nothing' = "Nothing'"
    show (Just' a) = "Just' " ++ show a




class YesNo a where 
    yesno :: a -> Bool 

instance YesNo Int where 
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where 
    yesno [] = False
    yesno _ = True

instance YesNo Bool where 
    yesno = id 

instance YesNo (Maybe a)  where 
    yesno Nothing = False
    yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf y r1 r2 =
    if yesno y 
        then r1 
        else r2 

