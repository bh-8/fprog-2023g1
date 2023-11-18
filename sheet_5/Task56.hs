{--
trimap (show) (+1) (replicate 3 . show) (Triple 2 3 4)
trifoldr (+) (*) (^) 2 (Triple 1 2 3)
--}
data Triple a b c = Triple a b c deriving Show

class Trifunctor p where
trimap :: (a -> b) -> (c -> d) -> (e -> f) -> Triple a c e -> Triple b d f
trimap f g h (Triple x y z) = Triple (f x) (g y) (h z)

class Trifoldable p where
trifoldr :: (a -> d -> d) -> (b -> d -> d) -> (c -> d -> d) -> d -> Triple a b c -> d
trifoldr f g h i (Triple x y z) = f x (g y (h z i))
