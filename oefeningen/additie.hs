-- Berekenen van de som van twee getallen

add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = do
    print (add 3 5)