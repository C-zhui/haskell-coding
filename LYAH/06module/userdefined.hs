import qualified Geometry as G
import qualified Geometry.Sphere as S


a = G.sphereArea 3

main = putStrLn $ show a

f :: Float -> Float 
f v = 
    let a = 123.123 + v 
    in 456.123 + a

g = do 
    putStrLn "in f" 
    putStrLn "in f2" 
    putStrLn "in f3" 