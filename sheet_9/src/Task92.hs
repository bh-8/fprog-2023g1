module Main where
    import System.Random
    import GHC.IO.Handle (hFlush)
    import System.IO (stdout)
    import Data.List (intercalate)
    import Numeric (showBin)
    
    bingoList :: Int -> IO [Int]
    bingoList 0 = return []
    bingoList n = do
        r  <- randomRIO (0, 99)
        rs <- bingoList (n - 1)
        return (r:rs) 

    showBingoGrid :: [Int] -> String
    showBingoGrid l =
        " | " ++ show (l !! 0) ++ " | " ++ show (l !! 1) ++ " | " ++ show (l !! 2) ++ " | " ++ show (l !! 3) ++ " | " ++ show (l !! 4) ++ " | \n" ++
        "----------------------------\n" ++
        " | " ++ show (l !! 5) ++ " | " ++ show (l !! 6) ++ " | " ++ show (l !! 7) ++ " | " ++ show (l !! 8) ++ " | " ++ show (l !! 9) ++ " | \n" ++
        "----------------------------\n" ++
        " | " ++ show (l !! 10) ++ " | " ++ show (l !! 11) ++ " | free | " ++ show (l !! 12) ++ " | " ++ show (l !! 13) ++ " | \n" ++
        "----------------------------\n" ++
        " | " ++ show (l !! 14) ++ " | " ++ show (l !! 15) ++ " | " ++ show (l !! 16) ++ " | " ++ show (l !! 17) ++ " | " ++ show (l !! 18) ++ " | \n" ++
        "----------------------------\n" ++
        " | " ++ show (l !! 19) ++ " | " ++ show (l !! 20) ++ " | " ++ show (l !! 21) ++ " | " ++ show (l !! 22) ++ " | " ++ show (l !! 23) ++ " | \n"

    bingoLists :: Int -> IO [[Int]]
    bingoLists 0 = return []
    bingoLists n = do
        bl <- bingoList 24
        bls <- bingoLists (n - 1)
        return (bl:bls)

    bingoMaker :: IO ()
    bingoMaker = do
        putStr "How many bingo cards should be generated? "
        hFlush stdout
        input_n <- getLine
        let n = read input_n :: Int
        if n > 0 then do
            d <- bingoLists n
            let s = intercalate "\n\n\n" (map showBingoGrid d)
            writeFile "Bingos.txt" s
            putStrLn "File 'Bingos.txt' written."
            hFlush stdout
        else do
            putStrLn "Error: Not a valid number!"
            hFlush stdout

    main :: IO ()
    main = do
        bingoMaker
