-- Om Dijkstra's algoritme te laten werken, moet je bijhouden hoe ver elke node van de start node afligt.

afstandTabel =  [
                ('A', 0),
                ('B', 999999),
                ('C', 999999),
                ('D', 999999) ]
-- In dit geval is 999999 oneindig

afstandBijwerken :: Char -> Int -> [(Char, Int)] -> [(Char, Int)]
afstandBijwerken node afstand afstandTabel =
    case lookup node afstandTabel of 
        Nothing -> error "Node niet gevonden"
        Just x -> map (\(n, a) -> if n == node then (n, afstand) else (n, a)) afstandTabel

-- a = afstand, n = node, x = huidige afstand (maar wordt niet gebruikt)


main :: IO ()
main = do
    let bijgewerkteTabel1 = afstandBijwerken 'B' 2 afstandTabel
    let bijgewerkteTabel2 = afstandBijwerken 'C' 5 bijgewerkteTabel1
    -- Hier maak ik gebruik van let om de bijwerkingen op te slaan.

    print (bijgewerkteTabel1)
    print (bijgewerkteTabel2)
