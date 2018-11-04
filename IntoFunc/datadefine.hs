data Day = Mon|Tue|Wed|Thu|Fri|Sat|Sun deriving(Show,Eq,Ord,Enum)

tomorrow :: Day ->Day
tomorrow Sun = Mon
tomorrow d = succ d

yesterday :: Day->Day
yesterday Mon = Sun
yesterday d = pred d

type Name = String 
type Author = String
type ISBN = String 
type Price = Double 

data Book = 
    Book{
        name::Name,
        author ::Author,
        isbn::ISBN,
        price::Price
    }
    deriving (Show,Eq)
instance Ord Book where
    (<=) a b  = price a <= price b

book1 = Book "programming in haskell " "Alen" "123-576" 99.99

(|>) :: a->(a->b)->b
(|>) = flip ($)


