import qualified Data.Map as M 
data Person = Person{
    name :: String,
    age :: Int
}deriving (Eq,Show,Read)

p_persons = [
    Person {name = "Mike", age = 21},
    Person {name = "David", age = 19},
    Person {name = "Mery", age = 20},
    Person {name = "Losy", age = 22},
    Person {name = "Wendy", age = 11}
    ]

p_Mike = Person {name = "Mike", age = 21}

res = p_Mike `elem` p_persons

p_Nancy = read "Person {name    = \"Nancy\",  age= 11}" :: Person

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Staurday | Sunday 
    deriving(Show,Read,Eq,Enum,Ord,Bounded)

type DayType = Day


type AssocList k v = [(k,v)]

al :: AssocList String Int
al = [("hi",1),
    ("two",2)
    ]

type IntMap v = M.Map Int v
-- or IntMap = M.Map Int     (becase of curry)

data Either' a b = Left' a | Right' b deriving (Eq, Ord, Read, Show)

