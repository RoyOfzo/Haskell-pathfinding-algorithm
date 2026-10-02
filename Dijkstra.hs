-- Dijkstra's algoritme in Haskell voor de Paradigma opdracht.

import Data.List (foldl')

import Graph
import Neighbours
import Distance

infinity :: Weight
infinity = maxBound :: Int
-- Hier maak ik gebruik van de functie maxBound, zodat het algoritme niet meer afhankelijk is van een 'magic number' en zo min mogelijk kans heeft op conflicten met hoge waarden.

-- Hier wordt de afstandentabel opgebouwd. Het is een lijst van nodes en hun afstand tot de startNodenode.
-- In het begin is de startNodenode 0 en alle andere nodes oneindig, omdat ze nog niet bezocht zijn.
initialDistances :: Node -> Graph -> [(Node, Weight)]
initialDistances startNode graph =
    [(node, if node == startNode then 0 else infinity) | node <- nodes graph]

-- Voert relax uit op alle buren van de meegegeven node 
relaxNeighbours :: Node -> [(Node, Weight)] -> Graph -> [(Node, Weight)]
relaxNeighbours node distances graph =
        foldl' updateDistanceInTable distances (neighbours graph node)
    where
        currentDistance = getDistance node distances
        updateDistanceInTable table (neighbour, weight) =
                let
                    oldDistance = getDistance neighbour table
                    newDistance = relax currentDistance weight oldDistance
                in updateDistance neighbour newDistance table
-- foldl' is een high order functie die een lijst omzet naar één enkele waarde door het van links naar rechts toe te passen op de lijst.
-- foldl' wordt hier gebruikt om de afstandentabel bij te werken voor alle buren van de meegegeven node.
-- Ik heb ook expres gekozen voor foldl' in plaats van foldl. foldl' is strikt en foldl is lui en maakt 'grote thunks', wat voor een stack overflow kan leiden bij grote lijsten.

-- De dijkstra-functie berekent de kortste afstand vanaf een startNode ...
dijkstra :: Node -> Node -> Graph -> [(Node, Weight)]
dijkstra startNode endNode graph = 
    process (initialDistances startNode graph) []
    where
        process distances visited
            | null unvisited = distances                            -- Geen onbezochte nodes meer
            | node == endNode = distances                          -- Als de eindNode is bereikt
            | distance == infinity = distances                       -- Als de kleinste afstand oneindig is, zijn er geen andere mogelijke opties meer
            | otherwise = process newDistances (node : visited)  -- Wanneer er nog wel mogelijke nodes zijn
            where
                unvisited = filter (\entry -> fst entry `notElem` visited) distances 
                (node, distance) = findSmallestDistance unvisited
                newDistances = relaxNeighbours node distances graph
-- notElem wordt hier gebruikt om te controleren of de node al in de lijst van bezochte nodes zit.
-- fst wordt gebruikt om de eerste waarde van een node te krijgen.

-- Berekent de route door vanaf de doel node terug te rekenen naar de start node
route :: Node -> Node -> Graph -> [Node]
route startNode endNode graph = reverse (findPath endNode)                     -- De reverse functie wordt hier gebruikt om de route van start naar eind te weergeven
  where
        distances = dijkstra startNode endNode graph
        findPath node
            | node == startNode = [startNode]                                                       -- Als deze de start node is, dan is het compleet
            | otherwise =                                                                           -- Als het niet de start node is...
                    case [ source | (source, target, weight) <- graph, target == node,              -- ...Dan wordt hier gekeken welke node overeenkomt met de afkomst, doel en gewicht. 
                                getDistance source distances + weight == getDistance node distances ] of 
                                (parent:_) -> node : findPath parent                                -- De eerste :_ wordt gebruikt om alleen de eerste item van de lijst te pakken en de rest te negeren
                            [] -> error "No path found"                                             -- De tweede : wordt gebruikt om de node toe te voegen aan de lijst, maar omdat hier node ervoor moet staan, 
                                                                                                    -- wordt de lijst van eind naar start opgebouwd.
        
main :: IO ()
main = do
    print (route 'A' 'F' graph)
    print (getDistance 'F' (dijkstra 'A' 'F' graph))