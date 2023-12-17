module Task94 where
    State s a = State runState :: s -> (a, s)

    step :: State Field String
    walk :: Field -> (String, Field)

    main :: IO ()
    main = do
        g <- fromList [((i, j), False) | i <- [10..10], j <- [10..10]]
        f st (runState (replicateM 10 step) (Field g (Ant (0, 0) Up)))
