{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where
    import Deque
    import Data.Maybe
    import Test.Tasty
    import Test.Tasty.QuickCheck as QC

    empty_deque = Deque []
    any_deque = Deque [2, 3, 4]
    any_value = 1

    -- axiom 1
    axiom_push_front = peekFront (pushFront any_value any_deque) == Just any_value

    -- axiom 2
    axiom_push_front_empty = isNothing (peekFront empty_deque)

    -- axiom 3
    axiom_push_end = peekEnd (pushEnd any_value any_deque) == Just any_value

    -- axiom 4
    axiom_push_end_empty = isNothing (peekEnd empty_deque)

    -- axiom 5
    axiom_pop_front = pushFront (fromJust (peekFront any_deque)) (popFront any_deque) == any_deque

    -- axiom 6
    axiom_pop_end = pushEnd (fromJust (peekEnd any_deque)) (popEnd any_deque) == any_deque

    -- test properties
    prop_axiom_push_front = QC.testProperty "push_front" $ \v d ->
        peekFront (pushFront v d) == Just v

    prop_axiom_push_end = QC.testProperty "push_end" $ \v d ->
        peekEnd (pushEnd v d) == Just v

    prop_axiom_pop_front = QC.testProperty "pop_front" $ \d ->
        pushFront (fromJust (peekFront d)) (popFront d) == d

    prop_axiom_pop_end = QC.testProperty "pop_end" $ \d ->
        pushEnd (fromJust (peekEnd d)) (popEnd d) == d

    main :: IO ()
    main = defaultMain $ testGroup "Task71"
        [prop_axiom_push_front, prop_axiom_push_end, prop_axiom_pop_front, prop_axiom_pop_end]
