-- Deze module is verantwoordelijk voor alle functies rondom buren.
module Buren (buren isBezocht relax) where


buren :: Char -> [(Char, Int)]
buren node = [ (n, w) | (x, n, w) <- graph, x == node]


bezochteNodes = ['A','B']

isBezocht :: Char -> [Char] -> Bool
isBezocht node bezochteNodes = node `elem` bezochteNodes


relax :: Int -> Int -> Int -> Int
relax huidigeAfstand edgeGewicht oudeAfstand =
    min (huidigeAfstand + edgeGewicht) oudeAfstand