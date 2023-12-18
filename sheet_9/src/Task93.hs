module Main where
    import System.Random
    import GHC.IO.Handle (hFlush)
    import System.IO (stdout)
    import Data.List (intercalate)
    import Numeric (showBin)
    import Data.Map
    import System.Directory (doesFileExist)
    
    bingoList :: Int -> [(Int, String)] -> IO [String]
    bingoList 0 _ = return []
    bingoList n m = do
        r  <- randomRIO (1, length m)
        let s = fromList m ! r
        rs <- bingoList (n - 1) m
        return (s:rs)

    showBingoGrid :: [String] -> String
    showBingoGrid l =
        " | " ++ (l !! 0) ++ " | " ++ (l !! 1) ++ " | " ++ (l !! 2) ++ " | " ++ (l !! 3) ++ " | " ++ (l !! 4) ++ " | \n" ++
        "----------------------------\n" ++
        " | " ++ (l !! 5) ++ " | " ++ (l !! 6) ++ " | " ++ (l !! 7) ++ " | " ++ (l !! 8) ++ " | " ++ (l !! 9) ++ " | \n" ++
        "----------------------------\n" ++
        " | " ++ (l !! 10) ++ " | " ++ (l !! 11) ++ " | free | " ++ (l !! 12) ++ " | " ++ (l !! 13) ++ " | \n" ++
        "----------------------------\n" ++
        " | " ++ (l !! 14) ++ " | " ++ (l !! 15) ++ " | " ++ (l !! 16) ++ " | " ++ (l !! 17) ++ " | " ++ (l !! 18) ++ " | \n" ++
        "----------------------------\n" ++
        " | " ++ (l !! 19) ++ " | " ++ (l !! 20) ++ " | " ++ (l !! 21) ++ " | " ++ (l !! 22) ++ " | " ++ (l !! 23) ++ " | \n"

    bingoLists :: Int -> [(Int, String)] -> IO [[String]]
    bingoLists 0 _ = return []
    bingoLists n m = do
        bl <- bingoList 24 m
        bls <- bingoLists (n - 1) m
        return (bl:bls)

    bingoListToMap :: [String] -> Int -> [(Int, String)]
    bingoListToMap [] _ = []
    bingoListToMap (w:wl) i = (i, w):(bingoListToMap wl (i - 1))

    bingoMaker :: IO ()
    bingoMaker = do
        putStr "Source file: "
        hFlush stdout
        input_file <- getLine
        file_existance <- doesFileExist input_file
        if file_existance then do
            file_content <- readFile input_file
            let buzzword_list = words file_content
            let buzzword_map = bingoListToMap buzzword_list (length buzzword_list)
            
            putStr "How many bingo cards should be generated? "
            hFlush stdout
            input_n <- getLine
            let n = read input_n :: Int
            if n > 0 then do
                d <- bingoLists n buzzword_map
                let s = intercalate "\n\n\n" (Prelude.map showBingoGrid d)
                writeFile "Bingos.txt" s
                putStrLn "File 'Bingos.txt' written."
                hFlush stdout
            else do
                putStrLn "Error: Not a valid number!"
                hFlush stdout
        else do
            putStrLn ("Error: Could not access file '" ++ input_file ++ "'!")
            hFlush stdout

    main :: IO ()
    main = do
        bingoMaker
