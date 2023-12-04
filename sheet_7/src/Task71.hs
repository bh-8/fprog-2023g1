{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where
    import Deque
    import Data.Maybe
    import Test.Tasty
    import Test.Tasty.QuickCheck as QC

    -- axiom 1
    axiom_push_front :: Int -> Deque -> Bool
    axiom_push_front v d = fromJust (peekFront (pushFront v d)) == v

    -- axiom 2
    axiom_push_end :: Int -> Deque -> Bool
    axiom_push_end v d = fromJust (peekEnd (pushEnd v d)) == v

    -- axiom 3
    axiom_pop_front :: Deque -> Bool
    axiom_pop_front d = isEmpty d || (pushFront (fromJust (peekFront d)) (popFront d) == d)

    -- axiom 4
    axiom_pop_end :: Deque -> Bool
    axiom_pop_end d = isEmpty d || (pushEnd (fromJust (peekEnd d)) (popEnd d) == d)

    -- axiom 5
    axiom_peek_front :: Deque -> Bool
    axiom_peek_front (Deque d) = null d || fromJust (peekFront (Deque d)) == Prelude.head d

    -- axiom 6
    axiom_peek_end :: Deque -> Bool
    axiom_peek_end (Deque d) = null d || fromJust (peekEnd (Deque d)) == Prelude.last d

    -- axiom 7
    axiom_make_deque :: Deque -> Bool
    axiom_make_deque (Deque dl) = makeDequeFromList dl == Deque dl

    -- test properties
    prop_axiom_push_front = QC.testProperty "push_front" axiom_push_front

    prop_axiom_push_end = QC.testProperty "push_end" axiom_push_end

    prop_axiom_pop_front = QC.testProperty "pop_front" axiom_pop_front

    prop_axiom_pop_end = QC.testProperty "pop_end" axiom_pop_end

    prop_axiom_peek_front = QC.testProperty "peek_front" axiom_peek_front

    prop_axiom_peek_end = QC.testProperty "peek_end" axiom_peek_end

    prop_axiom_make_deque = QC.testProperty "make_deque" axiom_make_deque

    main :: IO ()
    main = defaultMain $ testGroup "Task71"
        [prop_axiom_push_front, prop_axiom_push_end, prop_axiom_pop_front, prop_axiom_pop_end, prop_axiom_peek_front, prop_axiom_peek_end, prop_axiom_make_deque]
