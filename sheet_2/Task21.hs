{--
buy 166
count (buy 166)
payoff (buy 166)
--}

data Jeton = Red | Green | Blue | Silver | Gold
    deriving (Show) -- (1)
data Jetons = Empty | Jeton :+ Jetons
    deriving (Show) -- (2)

value :: Jeton -> Int -- (1)
value v = case v of
    Red -> 1
    Green -> 5
    Blue -> 10
    Silver -> 50
    Gold -> 100

count :: Jetons -> Int -- (2)
count Empty         = 0
count (j :+ jc)     = 1 + (count jc)

payoff :: Jetons -> Int -- (2)
payoff Empty        = 0
payoff (j :+ jc)    = (value j) + (payoff jc)

buy' :: Int -> Jetons -> Jetons -- (3)
buy' amt jc
    | amt >= 100    = buy' (amt - 100) (Gold :+ jc)
    | amt >= 50     = buy' (amt - 50) (Silver :+ jc)
    | amt >= 10     = buy' (amt - 10) (Blue :+ jc)
    | amt >= 5      = buy' (amt - 5) (Green :+ jc)
    | amt >= 1      = buy' (amt - 1) (Red :+ jc)
    | otherwise     = jc

buy :: Int -> Jetons -- (3)
buy amt = buy' amt Empty
