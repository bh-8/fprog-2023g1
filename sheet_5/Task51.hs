{--
take 11 (partsum [1..])
take 6 (partsum harmonic)
--}

-- (1)
ev = [x | x <- [0..], (\n -> mod n 2 == 0) x]

-- (2)
harmonic = [(\n -> 1 / (fromIntegral n)) x | x <- [0..], (\n -> not (n == 0)) x]

-- (3)
palin = [x | x <- [0..], (\n -> (read . reverse . show) n == n) x]

-- (4)
partsum xs = [(\n -> foldr (+) 0 (take n xs)) x | x <- [0..]]
