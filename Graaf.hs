module Graaf where

import Data.List (nub)

-- Ik heb gekozen om hier aliases te gebruiken, om de code meer leesbaar te maken. 
-- Daarnaast heb ik de graaf termen (zoals node en weight) in het Engels gelaten.
type Node = Char
type Weight = Int
type Graaf = [(Node, Node, Weight)]

graaf :: Graaf
graaf = [
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
nodes :: Graaf -> [Node]
nodes graaf = nub [node | (from, to, _) <- graaf, node <- [from, to]]