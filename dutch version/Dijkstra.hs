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

-- De dijkstra-functie berekent de kortste afstand vanaf een gekozen start node (startNode) naar de gekozen eind node (eindNode)
dijkstra :: Node -> Node -> Graaf -> [(Node, Weight)]
dijkstra startNode eindNode graaf = 
    verwerk (eersteAfstanden startNode graaf) []
    where
        verwerk afstanden bezocht
            | null onbezocht = afstanden                            -- Geen onbezochte nodes meer
            | node == eindNode = afstanden                          -- Als de eindNode is bereikt
            | afstand == oneindig = afstanden                       -- Als de kleinste afstand oneindig is, zijn er geen andere mogelijke opties meer
            | otherwise = verwerk nieuweAfstanden (node : bezocht)  -- Wanneer er nog wel mogelijke nodes zijn
            where
                onbezocht = filter (\regel -> fst regel `notElem` bezocht) afstanden 
                (node, afstand) = vindKleinsteAfstand onbezocht
                nieuweAfstanden = relaxBuren node afstanden graaf
-- notElem wordt hier gebruikt om te controleren of de node al in de lijst van bezochte nodes zit.
-- fst wordt gebruikt om de eerste waarde van een node te krijgen.

-- Berekent de route door vanaf de doel node terug te rekenen naar de start node
route :: Node -> Node -> Graaf -> [Node]
route startNode eindNode graaf = reverse (vindPad eindNode)                     -- De reverse functie wordt hier gebruikt om de route van start naar eind te weergeven
  where
    afstanden = dijkstra startNode eindNode graaf
    vindPad node
      | node == startNode = [startNode]                                         -- Als deze de start node is, dan is het compleet
      | otherwise =                                                             -- Als het niet de start node is...
          case [ afkomst | (afkomst, doel, gewicht) <- graaf, doel == node,     -- ...Dan wordt hier gekeken welke node overeenkomt met de afkomst, doel en gewicht. 
                getAfstand afkomst afstanden + gewicht == getAfstand node afstanden ] of 
                (ouder:_) -> node : vindPad ouder                               -- De eerste :_ wordt gebruikt om alleen de eerste item van de lijst te pakken en de rest te negeren
                [] -> error "Geen pad gevonden"                                 -- ^ De tweede : wordt gebruikt om de node toe te voegen aan de lijst, maar omdat hier node ervoor moet staan, wordt de lijst van eind naar start opgebouwd.
        
main :: IO ()
main = do
    print (route 'A' 'F' graaf)
    print (getAfstand 'F' (dijkstra 'A' 'F' graaf))