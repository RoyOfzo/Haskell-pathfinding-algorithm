module Graaf where

-- Ik heb gekozen om hier aliases te gebruiken, om de code meer leesbaar te maken. 
-- Daarnaast heb ik de graaf termen (zoals node en weight) in het Engels gelaten.
type Node = Char
type Weight = Int
type Graaf = [(Node, Node, Weight)]


graaf = [
        ('A', 'B', 2),
        ('A', 'C', 5),
        ('B', 'C', 1),
        ('B', 'D', 3),
        ('C', 'D', 2) ]

-- Haalt alle unieke nodes uit de graaf en geeft deze terug als een lijst.
-- Dit is ook meteen wat nub doet, het haalt alle dubbele nodes uit de lijst.
nodes :: Graaf -> [Node]
nodes graaf = nub [node | (from, to, _) <- graaf, node <- [from, to]]