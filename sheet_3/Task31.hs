{--
mSplitL [2,4,6] "foobar"
--}

-- returns elements between index i and j
subL :: Int -> Int -> [a] -> [a] -> [a]
subL i j lst acc
    | i > j     = [] -- invalid case
    | i > 0     = subL (i - 1) (j - 1) (tail lst) [] -- cut off first i elements
    | null lst  = acc -- if j exceeds lst length
    | j == 0    = acc -- done
    | otherwise = subL i (j - 1) (tail lst) (acc ++ [head lst]) -- copy next element to acc

splitL :: Int -> [a] -> [[a]]
splitL i x = [subL 0 i x [], subL i (length x) x []]

mSplitL' :: Int -> [Int] -> [a] -> [[a]] -> [[a]]
mSplitL' c splits dat acc
    | null splits = acc ++ [subL c (length dat) dat []] -- return acc if all splits are processed
    | otherwise = mSplitL' (head splits) (tail splits) dat (acc ++ [subL c (head splits) dat []])

mSplitL :: [Int] -> [a] -> [[a]]
mSplitL splits dat = mSplitL' 0 splits dat []
