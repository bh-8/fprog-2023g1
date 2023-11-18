import Data.Bifunctor
import Data.Bifoldable

{--
bimap (+2) (show) (Pair 1 10)
bimap (+1) (+2) (Left_ 1)
bimap (+1) (+2) (Right_ 1)
bifoldr (+) (*) 3 (Left_ 1)
bifoldr (+) (*) 3 (Right_ 1)
bifoldr (+) (*) 3 (Pair 2 10)
--}

data Either_ a b = Left_ a | Right_ b  deriving Show
instance Bifunctor Either_ where
    bimap :: (a -> b) -> (c -> d) -> Either_ a c -> Either_ b d
    bimap f g (Left_ x) = Left_ (f x)
    bimap f g (Right_ x) = Right_ (g x)
instance Bifoldable Either_ where
    bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> Either_ a b -> c
    bifoldr f g i (Left_ x) = f x i
    bifoldr f g i (Right_ x) = g x i

data Pair a b = Pair a b deriving Show
instance Bifunctor Pair where
    bimap :: (a -> b) -> (c -> d) -> Pair a c -> Pair b d
    bimap f g (Pair x y) = Pair (f x) (g y)
instance Bifoldable Pair where
    bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> Pair a b -> c
    bifoldr f g i (Pair x y) = f x (g y i)
