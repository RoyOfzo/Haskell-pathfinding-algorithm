-- Berekenen van het kleinste getal in een lijst en tussen twee getallen

berekenKleinsteGetal :: Int -> Int -> Int
berekenKleinsteGetal x y 
    | x < y = x
    | otherwise = y

berekenKleinsteGetalUitArray :: [Int] -> Int
berekenKleinsteGetalUitArray [x] = x 
berekenKleinsteGetalUitArray (x:xs) = berekenKleinsteGetal x (berekenKleinsteGetalUitArray xs)


main :: IO ()
main = do
    print (berekenKleinsteGetal 12 6)
    print (berekenKleinsteGetalUitArray [12, 6, 3, 4, 15])