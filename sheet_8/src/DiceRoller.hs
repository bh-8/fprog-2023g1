module DiceRoller where
    import System.Random
    import Debug.Trace
    import System.IO
    
    roll :: Int -> IO Int
    roll bound = randomRIO (1, bound)

    mroll :: Int -> Int -> IO ()
    mroll bound 1 = do
        x <- roll bound
        putStr (show x)
        putStrLn "\nDone rolling!"
        hFlush stdout
    mroll bound i = do
        x <- roll bound
        putStr (show x ++ ", ")
        hFlush stdout
        mroll bound (i - 1)

    rollLoop :: IO ()
    rollLoop = do
        putStr "What kind of die should be rolled (q for quit)? "
        hFlush stdout
        input <- getLine
        if input == "q" then putStrLn "Done!"
        else do
            x <- roll (read input :: Int)
            putStrLn ("Rolled: " ++ show x)
            hFlush stdout
            rollLoop

    mrollLoop :: IO ()
    mrollLoop = do
        putStr "What kind of die should be rolled (q for quit)? "
        hFlush stdout
        input_kind <- getLine
        if input_kind == "q" then putStrLn "Done!"
        else do
            putStr "How many times should the die be rolled? "
            hFlush stdout
            input_times <- getLine
            mroll (read input_kind :: Int) (read input_times :: Int)
            mrollLoop
