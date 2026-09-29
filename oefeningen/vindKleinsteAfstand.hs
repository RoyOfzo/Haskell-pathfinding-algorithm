-- Hier zoek ik de kleinste afstand in de afstandTabel.

afstandTabel =  [
                ('A',0),
                ('B',2),
                ('C',5),
                ('D',999999) ]

-- kleinsteNode afstandTabel

vindKleinsteAfstand :: [(Char, Int)] -> (Char, Int)
vindKleinsteAfstand [] = error "Lege lijst"
vindKleinsteAfstand [(node, afstand)] = (node, afstand)
vindKleinsteAfstand ((node, afstand) : xs) =
    let (kleinsteNode, kleinsteAfstand) = vindKleinsteAfstand xs
    in if afstand < kleinsteAfstand
       then (node, afstand)
       else (kleinsteNode, kleinsteAfstand)


main :: IO ()
main = do
    print (vindKleinsteAfstand afstandTabel)