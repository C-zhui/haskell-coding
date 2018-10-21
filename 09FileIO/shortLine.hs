import qualified Control.Monad as M
import qualified Data.Char as C

main = do
    contents <- getContents
    putStr $ shortLinesOnly contents
shortLinesOnly :: String -> String
shortLinesOnly = unlines . map (\x -> x ++ show (length x)) . filter (\line -> length line < 10 ) . lines