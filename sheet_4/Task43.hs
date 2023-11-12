{--
take 10 ev
take 10 harmonic
take 10 triangle
take 20 palin
--}

nat = [0 ..]

-- (1)
ev = filter (\n -> mod n 2 == 0) nat

-- (2)
harmonic = map (\n -> 1 / (fromIntegral n)) (filter (\n -> not (n == 0)) nat)

-- (3)
triangle = map (\n -> foldr (+) 0 [0 .. n]) (filter (\n -> not (n == 0)) nat)

-- (4)
palin = filter (\n -> (read . reverse . show) n == n) nat
