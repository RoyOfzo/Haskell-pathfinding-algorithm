-- Stack string inverter (die niet werkt)

string = "stack";

whileLoop :: Int -> String
whileLoop x =
    if x < length string
        then
            whileLoop (x + 1) ++ [string !! (length string - 1 - x)]
        else
            ""

main :: IO ()
main = do
    let result = whileLoop 0
    print result