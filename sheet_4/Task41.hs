{--
product' [1 .. 5]
allOdd [5, 7]
allOdd [4]
xSquaredPlusThreeXPlusFive
xSquaredPlusThreeXPlusFive !! 1
getByKey [("a", 2), ("b", 3), ("a", 6), ("c", 4)] "a"
--}

product' :: [Int] -> Int
product' intlist = foldr (*) 1 intlist

allOdd :: [Int] -> Bool
allOdd intlist = foldr (&&) True (map (\i -> mod i 2 == 1) intlist)

xSquaredPlusThreeXPlusFive :: [(Integer, Integer)]
xSquaredPlusThreeXPlusFive = map (\x -> (x, x * x + 3 * x + 5)) [0 .. 150]

getByKey :: [(String, Int)] -> String -> [Int]
getByKey dict key = map (\(_, y) -> y) (filter (\(x, _) -> x == key) dict)
