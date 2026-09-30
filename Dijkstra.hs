-- Dijkstra's algoritme in Haskell voor de Paradigma opdracht.

import Graaf
import Buren
import Afstand

oneindig :: Weight
oneindig = 999999

-- Hier wordt de afstandentabel opgebouwd. Het is een lijst van nodes en hun afstand tot de startnode.
-- In het begin is de startnode 0 en alle andere nodes oneindig, omdat ze nog niet bezocht zijn.
eersteAfstanden :: Node -> Graaf -> [(Node, Weight)]
eersteAfstanden start graaf =
    [(node, if node == start then 0 else oneindig) | node <- nodes graaf]



-- Voert relax uit op alle buren van de meegegeven node 
relaxNeighbours :: 
relaxNeighbours 




dijkstra ::
dijkstra 

main :: IO ()
main = do

