-- Buren vinden in een graaf

graph = [
    ('A','B', 2),
    ('A', 'C', 5),
    ('B', 'C', 1),
    ('B', 'D', 3),
    ('C', 'D', 2) ]

neighbours :: Char -> [(Char, Int)]
neighbours node = [ (n, w) | (x, n, w) <- graph, x == node]

-- n = neighbor, w = weight, x = huidige node

main :: IO ()
main = do
    print ("Buren van B:")
    print (neighbours 'B')

    print ("Buren van A:")
    print (neighbours 'A')
