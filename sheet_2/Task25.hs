{--
liconcat (create [1,2,3,4,5]) (add 8 (add 7 (create [6])))
getList (liconcat (create [1,2,3,4,5]) (add 8 (add 7 (create [6]))))
getSum (liconcat (create [1,2,3,4,5]) (add 8 (add 7 (create [6]))))
--}

data ListInt = ListInt [Int] Int deriving(Show)

create' :: [Int] -> ListInt -> ListInt
create' l li
    | length l == 0 = li
    | otherwise     = create' (tail l) (add (head l) li)
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
