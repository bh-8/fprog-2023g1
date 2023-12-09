module Main where
    import System.Directory
    import System.IO
    import Data.Char (toUpper)
    import Data.List
    
    alter :: IO ()
    alter = do
        putStr "Filename: "
        hFlush stdout
        file <- getLine
        fileExists <- doesFileExist file
        if fileExists then do
            fileContent <- readFile file
            let processedData = map (\l -> map (\w -> map (\c -> toUpper c) (reverse w)) (words l)) (lines fileContent)
            let processedFileContent = intercalate "\n" (map (\w -> intercalate " " w) processedData)
            writeFile ("U." ++ file) processedFileContent
            putStrLn ("File 'U." ++ file  ++ "' written.")
            hFlush stdout
        else do
            putStrLn "File does not exist!"
            hFlush stdout

    main :: IO ()
    main = alter
