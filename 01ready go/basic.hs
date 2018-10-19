
--  rm * -Include *.exe,*.hi,*.o
main = putStrLn "hello world"

doubleMe x = x + x

-- 延后求值
doubleUs x y = x * 2 + y * 2

-- 强制求值，不延后
doubleUs' x y = doubleMe x + doubleMe y

doubleSmallNumber' x = (if x > 100 then x else x*2)+1

