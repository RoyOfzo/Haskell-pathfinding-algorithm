-- Deze module is verantwoordelijk voor alle functies rondom het berekenen van afstanden.
module Afstand ( berekenKleinsteGetal, berekenKleinsteGetalUitArray, nieuweAfstand, getAfstand, vindKleinsteAfstand, afstandBijwerken) where

berekenKleinsteGetal :: Int -> Int -> Int
berekenKleinsteGetal x y 
    | x < y = x
    | otherwise = y

berekenKleinsteGetalUitArray :: [Int] -> Int
berekenKleinsteGetalUitArray [x] = x 
berekenKleinsteGetalUitArray (x:xs) = berekenKleinsteGetal x (berekenKleinsteGetalUitArray xs)


nieuweAfstand :: Int -> Int -> Int
nieuweAfstand x y = x + y


isEven n = n `mod` 2 == 0

filterEvenGetal xs = filter isEven xs


getAfstand :: Char -> [(Char, Int)] -> Int
getAfstand node afstandTabel = 
    case lookup node afstandTabel of 
        Nothing -> error "Node niet gevonden"
        Just afstand -> afstand


vindKleinsteAfstand :: [(Char, Int)] -> (Char, Int)
vindKleinsteAfstand [] = error "Lege lijst"
vindKleinsteAfstand [(node, afstand)] = (node, afstand)
vindKleinsteAfstand ((node, afstand) : xs) =
    let (kleinsteNode, kleinsteAfstand) = vindKleinsteAfstand xs
    in if afstand < kleinsteAfstand
       then (node, afstand)
       else (kleinsteNode, kleinsteAfstand)



afstandBijwerken :: Char -> Int -> [(Char, Int)] -> [(Char, Int)]
afstandBijwerken node afstand afstandTabel =
    case lookup node afstandTabel of 
        Nothing -> error "Node niet gevonden"
        Just x -> map (\(n, a) -> if n == node then (n, afstand) else (n, a)) afstandTabel
