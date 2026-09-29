-- Quicksort implementatie

qSort [] = []
qSort (x:xs) = qSort smaller ++ [x] ++ qSort larger
    where 
        smaller = filter (< x) xs
        larger = filter (>= x) xs

        
main :: IO ()
main = do
    print (qSort [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5])
    