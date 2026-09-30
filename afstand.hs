-- Deze module bevat functies voor het lezen en het bijwerken van afstanden in een tabel.
module Afstand (getAfstand, vindKleinsteAfstand, afstandBijwerken) where

getAfstand :: Node -> [(Node, Weight)] -> Weight
getAfstand node afstandTabel = 
    case lookup node afstandTabel of 
        Nothing -> error "Node niet gevonden"
        Just afstand -> afstand

-- Vindt de node met de kleinste afstand in een lijst van nodes en afstanden op een recursieve manier. 
vindKleinsteAfstand :: [(Node, Weight)] -> (Node, Weight)
vindKleinsteAfstand [] = error "Lege lijst"
vindKleinsteAfstand [(node, afstand)] = (node, afstand)
vindKleinsteAfstand ((node, afstand) : xs) =
    let (kleinsteNode, kleinsteAfstand) = vindKleinsteAfstand xs
    in if afstand < kleinsteAfstand
       then (node, afstand)
       else (kleinsteNode, kleinsteAfstand)

-- Deze functie past afstanden per node aan in een afstandTabel. In realiteit past deze functie niet direct de tabel aan, maar maakt een nieuwe en geeft die terug.
afstandBijwerken :: Node -> Weight -> [(Node, Weight)] -> [(Node, Weight)]
afstandBijwerken node afstand afstandTabel =
    case lookup node afstandTabel of 
        Nothing -> error "Node niet gevonden"
        Just ha -> map (\(n, a) -> if n == node then (n, afstand) else (n, a)) afstandTabel
-- Ik heb hier gekozen om afkortingen te gebruiken voor de variabelen, omdat de regel anders te groot wordt.
-- a = afstand, n = node en ha = huidige afstand, maar wordt niet gebruikt. Soms komt het voor dat een underscore (_) wordt gebruikt voor ongebruikte variabelen, maar ik kan niet iets concreets vinden of dit ook echt geaccepteerd is in de Haskell best practices.

