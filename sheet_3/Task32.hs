-- Blatt 2

data ListInt = ListInt [Int] Int deriving(Show)

create' :: [Int] -> ListInt -> ListInt
create' l li
    | null l    = li
    | otherwise = create' (tail l) (add (head l) li)
                               -- call 'add' recursively 

create :: [Int] -> ListInt -- create new ListInt
create l = create' l (ListInt [] 0)

add :: Int -> ListInt -> ListInt -- add element to ListInt
add i (ListInt l s) = ListInt (l ++ [i]) (s + i)

liconcat :: ListInt -> ListInt -> ListInt -- concatenation
liconcat (ListInt l1 s1) (ListInt l2 s2) = ListInt (l1 ++ l2) (s1 + s2)

getList :: ListInt -> [Int] -- return list
getList (ListInt l s) = l

getSum :: ListInt -> Int -- return sum
getSum (ListInt l s) = s

-- Blatt 3

{--
sort [create [1,1,1,1,1], create [4,5,6], create [1,2,3]]
--}

-- define sort for all types which are comparable aka. deriving Ord
sort :: Ord a => [a] -> [a]
sort [] = []
sort (p:xs) = sort (lt xs) ++ [p] ++ sort (gt xs)
    where lt [] = []
          lt (y:ys) = if y<p then y:lt ys else lt ys
          gt [] = []
          gt (y:ys) = if y<p then gt ys else y:gt ys

-- two ListInt instances are considered equal when the sum of the containing elements is equal
instance Eq ListInt where
    (==) (ListInt l1 s1) (ListInt l2 s2) = s1 == s2

instance Ord ListInt where
    (<=) (ListInt l1 s1) (ListInt l2 s2)
        | length l1 == length l2 = s1 <= s2 -- when length is equal let sum decide
        | otherwise              = length l1 <= length l2
