import Numeric.Natural
import Data.Maybe
import Debug.Trace

{--
store [Component "C1" 8, Component "C2" 16, Component "C3" 4] "C4" 12
isProducible (Product "my product" [("Wood",3),("Stone",5)]) [Component "Wood" 10, Component "Stone" 10]
produce (Product "my product" [("Wood",3),("Stone",5)]) [Component "Wood" 10, Component "Stone" 10]
--}

data Component a = Component a Natural
    deriving Show
type Storage a = [Component a]
data Product a b = Product b [(a, Natural)] -- modified definition

-- returns the amount of a component in a storage if it exists
contains :: Eq a => Storage a -> a -> Maybe Natural
contains storage search
    | null storage = Nothing
contains (Component description amount:storageTail) search
    | description == search = Just amount
    | otherwise             = contains storageTail search

-- adds a component to a storage or adds its amount to an existing entry of the same component
store' :: Eq a => Storage a -> a -> Natural -> Storage a -> Storage a
store' storage search value acc
    | null storage = acc -- when storage processing is done
store' (Component description amount:storageTail) search value acc
    | description == search = store' storageTail search value (Component description (amount + value) : acc) -- if value has been found
    | otherwise             = store' storageTail search value (Component description amount : acc) -- skip
store :: Eq a => Storage a -> a -> Natural -> Storage a
store storage description amount
    | isNothing (contains storage description) = Component description amount : storage -- if component does not exist already
    | otherwise                                = store' storage description amount [] -- if value needs to be added onto existing

-- reduces the amount of a given component in a given storage
remove' :: Eq a => Storage a -> a -> Natural -> Storage a -> Storage a
remove' storage search value acc
    | null storage = acc -- when storage processing is done
remove' (Component description amount:storageTail) search value acc
    | description == search && amount - value <= 0 = remove' storageTail search value acc -- remove element if quantity is negative or zero
    | description == search                        = remove' storageTail search value (Component description (amount - value) : acc)
    | otherwise                                    = remove' storageTail search value (Component description amount : acc) -- skip
remove :: Eq a => Storage a -> a -> Natural -> Storage a
remove storage description amount
    | isNothing (contains storage description) = storage -- if component does not exist, nothing needs to be done
    | otherwise                                = remove' storage description amount [] -- if value has to be removed

-- checks if the storage has enough of the needed components
isProducible' :: (Eq a) => [(a, Natural)] -> Storage a -> Bool
isProducible' tlist storage
    | null tlist = True -- product without any requirements is always producible
isProducible' (tupel:tupelTail) storage
    | isNothing (contains storage (fst tupel))            = False -- if required component is not in storage
    | snd tupel > fromJust (contains storage (fst tupel)) = False -- if required amount exceeds storage capacity
    | null tupelTail                                      = True
    | otherwise                                           = isProducible' tupelTail storage
isProducible :: (Eq a, Eq b) => Product a b -> Storage a -> Bool
isProducible (Product name requiredComponents) = isProducible' requiredComponents

-- removes the used components from the storage
produce' :: (Eq a) => [(a, Natural)] -> Storage a -> Storage a
produce' requiredComponents storage
    | null requiredComponents      = storage -- product without any requirements is always producible
produce' (tupel:tupelTail) storage = produce' tupelTail (uncurry (remove storage) tupel)

produce :: (Eq a, Eq b) => Product a b -> Storage a -> Storage a
produce product storage
    | not (isProducible product storage) = storage
produce (Product name requiredComponents) storage = produce' requiredComponents storage
