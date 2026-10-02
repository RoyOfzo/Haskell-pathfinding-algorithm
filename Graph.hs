-- Deze module bevat de definitie van een graaf en een graaf die voorbeeld geeft van de zwakte van mijn algoritme.
module Graph where

import Data.List (nub)

-- Ik heb gekozen om hier aliases te gebruiken, om de code meer leesbaar te maken. 
type Node = Char
type Weight = Int
type Graph = [(Node, Node, Weight)]

graph :: Graph
graph = [
        ('A', 'B', 2),
        ('A', 'C', 5),
        ('B', 'C', 1),
        ('B', 'D', 3),
        ('B', 'E', 4),
        ('C', 'D', 2),
        ('C', 'E', 3),
        ('D', 'E', 1),
        ('E', 'F', 2),
        ('D', 'F', 1),
        ('B', 'D', 1) ]

-- Dit is een voorbeeld van een graaf die de zwakte van mijn algoritme laat zien. Het algoritme kiest de kortste afstand, maar als er meerdere paden zijn met dezelfde afstand, kiest die het eerste pad.
--     ('A', 'B', 2),
--     ('A', 'C', 5),
--     ('B', 'C', 1),
--     ('B', 'D', 3),
--     ('B', 'E', 4),
--     ('C', 'D', 2),
--     ('C', 'E', 3),
--     ('D', 'E', 1),
--     ('D', 'F', 6),
--     ('E', 'F', 2)

-- Haalt alle unieke nodes uit de graaf en geeft deze terug als een lijst.
-- Dit is ook meteen wat nub doet, het haalt alle dubbele nodes uit de lijst.
nodes :: Graph -> [Node]
nodes graph = nub [node | (source, target, _) <- graph, node <- [source, target]]