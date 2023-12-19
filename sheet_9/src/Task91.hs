{-# LANGUAGE InstanceSigs #-}
module Main where
    data Identity a = Identity a
    instance Functor Identity where
        fmap :: (a -> b) -> Identity a -> Identity b
        fmap f (Identity v) = Identity (f v)
    instance Applicative Identity where
        pure :: a -> Identity a
        pure = Identity
        (<*>) :: Identity (a -> b) -> Identity a -> Identity b
        (<*>) (Identity f) (Identity v)= Identity (f v)
    instance Monad Identity where
        return :: a -> Identity a
        return = pure
        (>>=) :: Identity a -> (a -> Identity b) -> Identity b
        (>>=) (Identity v) f = f v

    data CountBinds a = CountBinds (Integer, a)
    instance Functor CountBinds where
        fmap :: (a -> b) -> CountBinds a -> CountBinds b
        fmap f (CountBinds (i, v)) = CountBinds (i, f v)
    instance Applicative CountBinds where
        pure :: a -> CountBinds a
        pure v = CountBinds (0, v)
        (<*>) :: CountBinds (a -> b) -> CountBinds a -> CountBinds b
        (<*>) (CountBinds (i, f)) (CountBinds (j, v)) = CountBinds (j, f v)
    val :: (CountBinds a) -> a
    val (CountBinds (i, v)) = v
    instance Monad CountBinds where
        return :: a -> CountBinds a
        return = pure
        (>>=) :: CountBinds a -> (a -> CountBinds b) -> CountBinds b
        (>>=) (CountBinds (i, v)) f = 
            let CountBinds (i', result) = f v
            in CountBinds (i + i' + 1, result)

    binds :: CountBinds a -> Integer
    binds (CountBinds (i, v)) = i

    two :: CountBinds Int
    two = do
        x <- return 1
        y <- return 2
        return (x + y)

    main :: IO ()
    main = do
        putStrLn ("binds two = " ++ show (binds two))
