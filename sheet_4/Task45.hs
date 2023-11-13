{--
ttake 10 0 f_all
ttake 10 1 f_all
ttake 10 10 f_all
--}

nat = [0..]

f_a = map (\a -> (\x -> x * x + x + a)) nat

f_all = map (\fa -> map (\x -> fa x) nat) f_a

ttake n i f_all = take n (f_all !! i)
