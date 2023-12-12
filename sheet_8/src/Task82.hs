module Main where
    import SortedList
    import Test.Tasty.QuickCheck (sample, Arbitrary (arbitrary), Gen)

    main :: IO ()
    main = do
        sample (arbitrary :: Gen (SortedList Int))
