{--
minimum(s) = head(s) ∀(sorted lists s)
--> True

maximum(s) = head(s) ∀(sorted lists s)
--> False; maximum(s) = last(s) ∀(sorted lists s)

isSorted(x:s) = True ∀(sorted lists s and element x)
--> True

(x = head(s)) => length (s) = length (insert x s) ∀(sorted lists s and element x)
--> False; (x = head(s)) => length (s) + 1 = length (insert x s) ∀(sorted lists s and element x)

(x < head(s)) => (isSorted(x:s)) = True ∀(sorted lists s and element x)
--> True

(isSorted(x) ∧ isSorted(y)) => isSorted(x++y) ∀(sorted lists x and y)
--> True
--}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where
    import SortedList
    import Prelude hiding ((++))
    import Test.Tasty
    import Test.Tasty.QuickCheck as QC hiding (SortedList)

    axiom_minimum :: SortedList -> Bool
    axiom_minimum (SortedList sl) = Prelude.null sl || (foldr min maxBound sl == SortedList.head (SortedList sl))

    axiom_maximum :: SortedList -> Bool
    axiom_maximum (SortedList sl) = Prelude.null sl || (foldr max minBound sl == SortedList.last (SortedList sl))

    axiom_insert_sorted :: Int -> SortedList -> Bool
    axiom_insert_sorted i (SortedList sl) = isSorted (insert i (SortedList sl))

    axiom_insert_length :: Int -> SortedList -> Bool
    axiom_insert_length i sl = 1 + SortedList.length sl == SortedList.length (insert i sl)

    axiom_concat :: SortedList -> SortedList -> Bool
    axiom_concat sl1 sl2 = isSorted sl1 && isSorted sl2 && isSorted (sl1 ++ sl2)

    prop_axiom_minimum = QC.testProperty "minimum" axiom_minimum

    prop_axiom_maximum = QC.testProperty "maximum" axiom_maximum

    prop_axiom_insert_sorted = QC.testProperty "insert_sorted" axiom_insert_sorted

    prop_axiom_insert_length = QC.testProperty "insert_length" axiom_insert_length

    prop_axiom_concat = QC.testProperty "concat" axiom_concat

    main :: IO ()
    main = defaultMain $ testGroup "Task72"
        [prop_axiom_minimum, prop_axiom_maximum, prop_axiom_insert_sorted, prop_axiom_insert_length, prop_axiom_concat]
