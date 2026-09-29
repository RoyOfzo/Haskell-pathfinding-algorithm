-- Berekenen van de afstand tussen twee punten

newDistance :: Int -> Int -> Int
newDistance x y = x + y

main :: IO ()
main = do
    print (newDistance 3 5)
    print (newDistance 10 15)