-- Dijkstra's algoritme in Haskell voor de Paradigma opdracht.

import Data.List (foldl')

import Graaf
import Buren
import Afstand

oneindig :: Weight
oneindig = maxBound :: Int
-- Hier maak ik gebruik van de functie maxBound, zodat het algoritme niet meer afhankelijk is van een 'magic number' en zo min mogelijk kans heeft op conflicten met hoge waarden.

-- Hier wordt de afstandentabel opgebouwd. Het is een lijst van nodes en hun afstand tot de startNodenode.
-- In het begin is de startNodenode 0 en alle andere nodes oneindig, omdat ze nog niet bezocht zijn.
eersteAfstanden :: Node -> Graaf -> [(Node, Weight)]
eersteAfstanden startNode graaf =
    [(node, if node == startNode then 0 else oneindig) | node <- nodes graaf]

-- Voert relax uit op alle buren van de meegegeven node 
relaxBuren :: Node -> [(Node, Weight)] -> Graaf -> [(Node, Weight)]
relaxBuren node afstanden graaf =
        foldl' veranderAfstand afstanden (buren graaf node)
    where
        huidigeAfstand = getAfstand node afstanden
        veranderAfstand tabel (buurman, weight) =
                let
                    oudeAfstand = getAfstand buurman tabel
                    nieuweAfstand = relax huidigeAfstand weight oudeAfstand
                in afstandBijwerken buurman nieuweAfstand tabel
-- foldl' is een high order functie die een lijst omzet naar één enkele waarde door het van links naar rechts toe te passen op de lijst.
-- foldl' wordt hier gebruikt om de afstandentabel bij te werken voor alle buren van de meegegeven node.
-- Ik heb ook expres gekozen voor foldl' in plaats van foldl. foldl' is strikt en foldl is lui en maakt 'grote thunks', wat voor een stack overflow kan leiden bij grote lijsten.

-- De dijkstra-functie berekent de kortste afstand vanaf een startNode ...
dijkstra :: Node -> Graaf -> [(Node, Weight)]
dijkstra startNode graaf = verwerk (eersteAfstanden startNode graaf) []
    where
        verwerk afstanden bezocht
            | null onbezocht = afstanden        -- Geen onbezochte nodes meer
            | afstand == oneindig = afstanden   -- Als de kleinste afstand oneindig is, zijn er geen andere mogelijke opties meer
            | otherwise =                       -- Wanneer er nog wel mogelijke nodes zijn
                verwerk nieuweAfstanden (node : bezocht)
            where
                onbezocht = filter (\regel -> fst regel `notElem` bezocht) afstanden 
                (node, afstand) = vindKleinsteAfstand onbezocht
                nieuweAfstanden = relaxBuren node afstanden graaf
-- notElem wordt hier gebruikt om te controleren of de node al in de lijst van bezochte nodes zit.
-- fst wordt gebruikt om de eerste waarde van een node te krijgen.


main :: IO ()
main = do
    print (dijkstra 'A' graaf)

 