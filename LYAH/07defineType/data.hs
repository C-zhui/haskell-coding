data Nat = Zero | Succ Nat deriving(Show,Eq)

zero = Zero

one = Succ Zero

intToNat::Int->Nat
intToNat 0 = Zero 
intToNat n = Succ (intToNat (n-1))

natToInt :: Nat->Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n


add :: Nat->Nat->Nat
add Zero n = n
add (Succ m) n = m `add` (Succ n)
 
data BoolExp = TRUE | FALSE | IF BoolExp BoolExp BoolExp 

eval :: BoolExp->Bool
eval TRUE = True
eval FALSE = False 
eval (IF con b1 b2) 
    | eval con == True = eval b1
    | otherwise = eval b2

newtype Velocity = Velocity Int
newtype Weight = Weight Int 
newtype Second = Second Int

instance Show Velocity where
    show (Velocity n) = show n ++ " m/s"

instance Show Weight where 
    show (Weight w) = show w ++ " kg"

instance Show Second where 
    show (Second s) = show s ++ "sec"

