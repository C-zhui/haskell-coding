module PutJSON where

import Data.List (intercalate)
import Json

renderJValue :: JValue -> String 
renderJValue (JString s) = show s
renderJValue (JNumber n ) = show n
renderJValue (JBool b) = if b then "true" else "false"
renderJValue JNull = "null"
renderJValue (JObject o) = "{"++ pair o ++  "}"
    where 
        pair [] = ""
        pair ps = intercalate "," (map renderPair ps)
        renderPair (k,v) = show k ++ ":"++ renderJValue v

renderJValue (JArray a) = "[" ++values a++"]" 
        where 
            values [] = ""
            values vs = intercalate "," (map renderJValue vs)
            
putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)