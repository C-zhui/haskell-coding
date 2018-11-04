module Main  where

import Json
import PutJSON

main = do
    let a =(JObject [("foo",JNumber 1),("bar",JBool False)]) 
    print a 
    putJValue a 
