-- Om Dijkstra's algoritme te implementeren, moet je bijhouden hoe ver elke node van de start node afligt. Hier oefen ik het uitlezen van een graaf.

afstandTabel =  [
                ('A', 0),
                ('B', 999999),
                ('C', 999999),
                ('D', 999999) ]
-- In dit geval is 999999 oneindig

getDistance :: Char -> [(Char, Int)] -> Int
getDistance node afstandTabel = 
    case lookup node afstandTabel of 
        Nothing -> error "Node niet gevonden"
        Just afstand -> afstand

main :: IO ()
main = do
    print (getDistance 'A' afstandTabel)
    print (getDistance 'B' afstandTabel)