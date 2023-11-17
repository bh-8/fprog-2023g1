-- added type 'a' to definition to match '* -> *'
data Bool_ a = True_ | False_
instance Functor Bool_ where
    fmap :: (a -> b) -> Bool_ a -> Bool_ b
    fmap f True_ = True_
    fmap f False_ = False_

data List a = List a (List a) | Empty
instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap f Empty = Empty
    fmap f (List i t) = List (f i) (fmap f t)

-- removed type 'b' to match '* -> *'
data Either_ a = Right_ a | Left_ a
instance Functor Either_ where
    fmap :: (a -> b) -> Either_ a -> Either_ b
    fmap f (Right_ x) = Right_ (f x)
    fmap f (Left_ x) = Left_ (f x)

data Maybe_ a = Nothing_ | Just_ a
instance Functor Maybe_ where
    fmap :: (a -> b) -> Maybe_ a -> Maybe_ b
    fmap f Nothing_ = Nothing_
    fmap f (Just_ x) = Just_ (f x)

-- removed type 'b' to match '* -> *'
data Pair a = Pair a a
instance Functor Pair where
    fmap :: (a -> b) -> Pair a -> Pair b
    fmap f (Pair x y) = Pair (f x) (f y)

data LList a = LList [a] (a, a) a
instance Functor LList where
    fmap :: (a -> b) -> LList a -> LList b
    fmap f (LList l p v) = LList (map f l) (f (fst p), f (snd p)) (f v)
