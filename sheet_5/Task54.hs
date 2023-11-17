{--
fmap (+3) (Seq 1 (Lseq [1, 2, 3] (Pseq (1, 2) End)))
foldr (+) 0 (Seq 1 (Lseq [1, 2, 3] (Pseq (1, 2) End)))
--}

data Seq a = End | Seq a (Seq a) | Lseq [a] (Seq a) | Pseq (a, a) (Seq a) deriving(Show)
instance Functor Seq where
    fmap :: (a -> b) -> Seq a -> Seq b
    fmap f End = End
    fmap f (Seq x s) = Seq (f x) (fmap f s)
    fmap f (Lseq xl s) = Lseq (map f xl) (fmap f s)
    fmap f (Pseq xp s) = Pseq (f (fst xp), f (snd xp)) (fmap f s)
instance Foldable Seq where
    foldr :: (a -> b -> b) -> b -> Seq a -> b
    foldr f i End = i
    foldr f i (Seq x s) = f x (foldr f i s)
    foldr f i (Lseq xl s) = foldr f (foldr f i s) xl
    foldr f i (Pseq xp s) = f (fst xp) (f (snd xp) (foldr f i s))
