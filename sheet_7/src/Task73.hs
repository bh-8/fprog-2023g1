module Main where
    import Debug.Trace

    {--
    trace :: String -> a -> a

    trace is not pure, as it has the side effect of printing a string to the console
    --}

    fib :: Int -> Int
    fib 0 = trace "fib(0) = 0" 0
    fib 1 = trace "fib(1) = 1" 1
    fib n = trace ("fib(" ++ show n ++ ") = fib(" ++ show (n - 1) ++ ") + fib(" ++ show (n - 2) ++ ")") (fib(n - 1) + fib(n - 2))

    main :: IO ()
    main = do
        print (fib 8)
