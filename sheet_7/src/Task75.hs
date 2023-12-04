module Main where
    import Test.Tasty
    import Test.Tasty.QuickCheck as QC hiding (SortedList)
    
    f_1 = reverse
    f_2 = (++)
    f_3 = (:)
    f_4 = head
    f_5 = tail
    f_6 = length

    axiom_1 :: [Int] -> Bool
    axiom_1 a = a == (f_1 . f_1) a

    axiom_2 :: [Int] -> [Int] -> Bool
    axiom_2 a b = f_2 (f_1 b) (f_1 a) == f_1 (f_2 a b)

    axiom_3 :: [Int] -> Bool
    axiom_3 a = if not (null a) then (a == f_3 (f_4 a) (f_5 a)) else True

    axiom_4 :: [Int] -> [Int] -> Bool
    axiom_4 a b = f_6 a + f_6 b == f_6 (f_2 a b)

    axiom_5 :: [Int] -> Bool
    axiom_5 a = if not (null a) then (f_2 ((f_1 . f_5) a) [f_4 a] == f_1 a) else True

    prop_axiom_1 = QC.testProperty "axiom_1" axiom_1
    prop_axiom_2 = QC.testProperty "axiom_2" axiom_2
    prop_axiom_3 = QC.testProperty "axiom_3" axiom_3
    prop_axiom_4 = QC.testProperty "axiom_4" axiom_4
    prop_axiom_5 = QC.testProperty "axiom_5" axiom_5

    main :: IO ()
    main = defaultMain $ testGroup "Task72"
        [prop_axiom_1, prop_axiom_2, prop_axiom_3, prop_axiom_4, prop_axiom_5]
