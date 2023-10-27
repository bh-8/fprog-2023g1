import Distribution.SPDX (LicenseId(App_s2p))
data Set a = Set [a]

createSet' :: Eq a => [a] -> [a] -> [a]
createSet' list acc
    | null list = acc
    | head list `elem` acc = createSet' (tail list) acc
    | otherwise            = createSet' (tail list) (head list : acc)

createSet :: Eq a => [a] -> Set a
createSet list = Set (createSet' list [])

union :: Eq a => Set a -> Set a -> Set a
union (Set l1) (Set l2) = Set (createSet' (l1 ++ l2) [])

intersection' :: Eq a => [a] -> [a] -> [a] -> [a]
intersection' l1 l2 acc
    | null l1           = acc
    | head l1 `elem` l2 = intersection' (tail l1) l2 (head l1 : acc)
    | otherwise         = intersection' (tail l1) l2 acc
intersection :: Eq a => Set a -> Set a -> Set a
intersection (Set l1) (Set l2) = Set (intersection' l1 l2 [])

-- l1 without l2
setMinus' :: Eq a => [a] -> [a] -> [a] -> [a]
setMinus' l1 l2 acc
    | null l1           = acc
    | head l1 `elem` l2 = setMinus' (tail l1) l2 acc -- dont accumulate
    | otherwise         = setMinus' (tail l1) l2 (head l1 : acc) -- accumulate
setMinus :: Eq a => Set a -> Set a -> Set a
setMinus (Set l1) (Set l2) = Set (setMinus' l1 l2 [])

isSubList' :: Eq a => [a] -> [a] -> Bool
isSubList' l1 l2
    | null l1           = True -- empty set is subset of any set
    | head l1 `elem` l2 = isSubList' (tail l1) l2
    | otherwise         = False
toString' :: Show a => [a] -> [Char] -> [Char]
toString' l s
    | null l = "{" ++ s ++ "}"
    | null s = toString' (tail l) (s ++ show (head l))
    | otherwise = toString' (tail l) (show (head l) ++ "," ++ s)
instance Eq a => Eq (Set a) where
    (==) (Set l1) (Set l2) = isSubList' l1 l2 && isSubList' l2 l1
instance Ord a => Ord (Set a) where
    (<=) (Set l1) (Set l2) = isSubList' l1 l2
instance Show a => Show (Set a) where
    show (Set l) = toString' l ""

{--
union (createSet [1,3,5,7]) (createSet [2,4,6,8])
intersection (createSet [1,2,3,4,5,6,7,8]) (createSet [0,2,4,6,8])
setMinus (createSet [1,2,3,4,5,6,7,8]) (createSet [1,3,5,7])
createSet [2,4,6] <= createSet [1,2,3,4,5,6]
createSet [3,6,9] <= createSet [1,2,3,4,5,6]
createSet [1,2,3] == createSet [3,2,1]
show (createSet [1,2,3,4])
show (createSet [createSet [1,2], createSet [3,4]])
--}
