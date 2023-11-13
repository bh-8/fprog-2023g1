{--
dfold (+) 0 (*) 1 [[1, 2, 3], [1, 4, 1], [2, 2, 2, 1]]

nfilter [even, odd] [1..100]
nfilter [even, (\x -> x > 20), (\x -> x < 40)] [1..100]
--}
dfold :: (b -> c -> c) -> c -> (a -> b -> b) -> b -> [[a]] -> c
dfold f i g j x = foldr f i (map (\y -> foldr g j y) x)

nfilter :: [(a -> Bool)] -> [a] -> [a]
nfilter p x = filter (\el -> foldr (&&) True (map (\ff -> ff el) p)) x
