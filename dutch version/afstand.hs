-- Deze module bevat functies voor het lezen en het bijwerken van afstanden in een tabel.
module Distance where

import Graph

-- Haalt de afstand van een node uit de afstandentabel.
getDistance :: Node -> [(Node, Weight)] -> Weight
getDistance node distances = 
    case lookup node distances of 
        Nothing -> error "Node not found"
        Just distance -> distance

-- Vindt de node met de kleinste afstand in een lijst van nodes en afstanden op een recursieve manier. 
findSmallestDistance :: [(Node, Weight)] -> (Node, Weight)
findSmallestDistance [] = error "Empty list"
findSmallestDistance [(node, distance)] = (node, distance)
findSmallestDistance ((node, distance) : xs) =
    let (smallestNode, smallestDistance) = findSmallestDistance xs
    in if distance < smallestDistance
       then (node, distance)
       else (smallestNode, smallestDistance)

-- Deze functie past afstanden per node aan in een afstandTabel. In realiteit past deze functie niet direct de tabel aan, maar maakt een nieuwe en geeft die terug.
updateDistance :: Node -> Weight -> [(Node, Weight)] -> [(Node, Weight)]
updateDistance node distance distanceTable =
    case lookup node distanceTable of 
         Nothing -> error "Node not found"
         Just cd -> map (\(n, d) -> if n == node then (n, d) else (n, weight)) distanceTable
-- Ik heb hier gekozen om afkortingen te gebruiken voor de variabelen, omdat de regel anders te groot wordt.
-- d = afstand, n = node en cd = huidige afstand, maar wordt niet gebruikt. Soms komt het voor dat een underscore (_) wordt gebruikt voor ongebruikte variabelen, maar ik kan niet iets concreets vinden of dit ook echt geaccepteerd is in de Haskell best practices.