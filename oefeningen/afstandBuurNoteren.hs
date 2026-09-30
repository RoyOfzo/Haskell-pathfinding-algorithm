-- Met deze oefening leer ik edge relaxation toe te passen, wat erg belangrijk is voor Dijkstra's algoritme.

relax :: Int -> Int -> Int -> Int
relax huidigeAfstand edgeGewicht oudeAfstand =
    if huidigeAfstand + edgeGewicht < oudeAfstand
    then huidigeAfstand + edgeGewicht
    else oudeAfstand
-- Deze functie wordt normaal in vergeleken

relaxMetMin :: Int -> Int -> Int -> Int
relaxMetMin huidigeAfstand edgeGewicht oudeAfstand =
    min (huidigeAfstand + edgeGewicht) oudeAfstand
-- Deze functie maakt gebruik van de (ingebouwde) min functie, wat het meer leesbaar maakt en meer geschikt is voor Haskell.

main :: IO ()
main = do
    print (relax 3 5 10) 
    print (relax 3 5 7) 

    print ("nu met min functie")
    print (relaxMetMin 3 5 10)
    print (relaxMetMin 3 5 7) 