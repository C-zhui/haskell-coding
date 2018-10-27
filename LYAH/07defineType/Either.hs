import qualified Data.Map as Map 

data LockerState = Taken | Free deriving(Show,Eq)

type Code = String 

type LockerMap = Map.Map Int (LockerState,Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup number lockermap = 
    case Map.lookup number lockermap of 
        Nothing -> Left $ "Locker number " ++ show number ++ " does not exist!"
        Just (state,code) -> if state == Free
            then Right code
            else Left $ "Locker " ++ show number ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

look1 = lockerLookup 101 lockers
look2 = lockerLookup 103 lockers
look3 = lockerLookup 109 lockers
