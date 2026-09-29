-- Om het Dijkstra algoritme te laten werken, moet het kunnen onthouden welke nodes al bezocht zijn. Daar ga ik hier mee oefenen

bezochteNodes = ['A','B']

isBezocht :: Char -> [Char] -> Bool
isBezocht node bezochteNodes = node `elem` bezochteNodes

main :: IO ()
main = do 
    print (isBezocht 'A' bezochteNodes)
    print (isBezocht 'C' bezochteNodes)