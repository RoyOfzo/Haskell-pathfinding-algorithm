-- Deze module is verantwoordelijk voor buren en het 'relaxen' van edges.
module Neighbours where

import Graph

-- Dit is een pure functie die de buren van een node teruggeeft, gepaard met de gewichten.
neighbours :: Graph -> Node -> [(Node, Weight)]
neighbours graph node = [
                    (neighbor, weight) |
                    (source, neighbor, weight) <- graph,
                     source == node]

isVisited :: Node -> [Node] -> Bool
isVisited node visitedNodes = node `elem` visitedNodes

-- Relax is een pure functie die de kortste afstand teruggeeft.
relax :: Weight -> Weight -> Weight -> Weight
relax currentDistance edgeWeight oldDistance =
    min (currentDistance + edgeWeight) oldDistance