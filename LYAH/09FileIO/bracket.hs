import System.IO
import Control.Exception

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' name mode f = 
    bracket (openFile name mode)
    (\handle -> hClose handle)
    (\handle -> f handle)

main = do
    bracket (openFile "output.txt" WriteMode)
        (\handle -> hClose handle)
        (\handle -> do
            line <- getContents
            hPutStrLn handle  line)