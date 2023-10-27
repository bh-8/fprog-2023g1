digit :: Int -> Int
digit x = if x == 0 then 0
    else (x `mod` 10) + (digit (x `div` 10))

digit2 :: Int -> Int
digit2 x = if x == 0 then 0
    else if x > 0 then (x `mod` 10) + (digit2 (x `div` 10))
    else (negate ((negate x) `mod` 10)) + (digit2 (negate ((negate x) `div` 10)))
