-- Kennis maken met de filter functie in Haskell

numbers = [1,2,3,4,5,6,7,8,9,10,11,12]

isEven n = n `mod` 2 == 0

filterEvenGetal xs = filter isEven xs

main :: IO ()
main = do
    print (filterEvenGetal numbers)
    