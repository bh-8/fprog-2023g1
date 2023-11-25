module MyVector where
    newtype Vector a = Vector [a] deriving Show

    vecFromList :: Num a => [a] -> Vector a
    vecFromList = Vector

    vecScalar :: Num a => a -> Vector a -> Vector a
    vecScalar s (Vector l) = Vector (map (*s) l)

    vecAdd :: Num a => Vector a -> Vector a -> Maybe (Vector a)
    vecAdd (Vector x) (Vector y) = if length x == length y
        then Just (Vector (map (\z -> (x !! z) + (y !! z)) [0..(length x - 1)]))
        else Nothing

    vecProd :: Num a => Vector a -> Vector a -> Maybe a
    vecProd (Vector x) (Vector y) = if length x == length y
        then Just (sum (map (\z -> (x !! z) * (y !! z)) [0..(length x - 1)]))
        else Nothing

    crossProd :: Num a => Vector a -> Vector a -> Maybe (Vector a)
    crossProd (Vector x) (Vector y) = if length x == 3 && length y == 3
        then Just (Vector [
            x !! 1 * y !! 2 - x !! 2 * y !! 1,
            x !! 2 * y !! 0 - x !! 0 * y !! 2,
            x !! 0 * y !! 1 - x !! 1 * y !! 0
        ])
        else Nothing
