import Prelude hiding (sum) -- allow definition of sum

sum :: Int -> Int
sum n = if n == 0 then 0 else n + sum (n-1)
