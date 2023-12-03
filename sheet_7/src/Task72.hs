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
    axiom_minimum sl = foldr min 999999 sl == SortedList.head sl

    prop_axiom_minimum = QC.testProperty "minimum" axiom_minimum

    prop_axiom_maximum = QC.testProperty "maximum" $ \s ->
        foldr max 0 s == SortedList.last s

    main :: IO ()
    main = defaultMain $ testGroup "Task72"
        [prop_axiom_minimum]
