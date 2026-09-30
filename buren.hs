-- Deze module is verantwoordelijk voor buren en het 'relaxen' van edges.
module Buren where

import Graaf (Graaf, Node, Weight)

-- Dit is een pure functie die de buren van een node teruggeeft, gepaard met de gewichten.
-- Deze functie is erg vergelijkbaar met die uit mijn oefening, maar krijgt nu een graaf mee.
buren :: Graaf -> Node -> [(Node, Weight)]
buren graaf node = [
                    (buurman, weight) |
                    (afkomst, buurman, weight) <- graaf,
                     afkomst == node]

isBezocht :: Node -> [Node] -> Bool
isBezocht node bezochteNodes = node `elem` bezochteNodes

-- Relax is een pure functie die de kortste afstand teruggeeft.
relax :: Weight -> Weight -> Weight -> Weight
relax currentDistance edgeWeight oldDistance =
    min (currentDistance + edgeWeight) oldDistance