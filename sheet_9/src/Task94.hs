module Main where
    import Data.Map
    import System.IO

    data Direction = Left | Right | Up | Down

    data Ant = Ant (Int, Int) Direction
    data Field = Field (Map (Int, Int) Bool) Ant

    --State s a = State runState :: s -> (a, s)


    --step :: State Field String
    --walk :: Field -> (String, Field)

    main :: IO ()
    main = do
        let g = fromList [((i, j), False) | i <- [1..10], j <- [1..10]]
        let f = Field g (Ant (0, 0) Up)
        putStrLn (show g)
        hFlush stdout
        --f st (runState (replicateM 10 step) (Field g (Ant (0, 0) Up)))
