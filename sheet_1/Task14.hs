digit :: Int -> Int
digit x
    | x == 0    = 0
    | x > 0     = (x `mod` 10) + digit (x `div` 10)
    | otherwise = (negate ((negate x) `mod` 10)) + digit (negate ((negate x) `div` 10))

isPrefixOf :: String -> String -> Bool
isPrefixOf s t
    | null s    = True
    | otherwise = (head s == head t) && isPrefixOf (tail s) (tail t)
