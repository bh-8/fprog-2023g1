import Prelude hiding (mconcat) -- allow definition of mconcat

{--
(mconcat (ML [WrapInt 1, WrapBool True, WrapString "Hello", WrapInt 2]) (prepend (ML [WrapInt 3]) (ML [WrapBool False, WrapString "World", WrapInt 4])))
extractBool
extractInt
extractString
countComponents
--}

-- list item type; allowing Int, String and Bool
data MLItem = WrapInt Int | WrapString String | WrapBool Bool
    deriving (Show)

-- ML handles list of MLItem
data ML = ML [MLItem]
    deriving (Show)

-- prepends the first element of ml1 to ml2
prepend :: ML -> ML -> ML
prepend (ML ml1) (ML ml2) = ML (head(ml1) : ml2)

-- concats two ML types
mconcat :: ML -> ML -> ML
mconcat (ML ml1) (ML ml2) = ML (ml1 ++ ml2)

-- integer
extractInt'' :: MLItem -> [Int]
extractInt'' (WrapInt int) = [int]
extractInt'' _ = []

extractInt' :: [Int] -> [MLItem] -> [Int]
extractInt' il ml = if length ml == 0 then il
    else extractInt' (il ++ (extractInt'' (head ml))) (tail ml)

extractInt :: ML -> [Int]
extractInt (ML ml) = extractInt' [] ml

-- string
extractString'' :: MLItem -> [String]
extractString'' (WrapString str) = [str]
extractString'' _ = []

extractString' :: [String] -> [MLItem] -> [String]
extractString' sl ml = if length ml == 0 then sl
    else extractString' (sl ++ (extractString'' (head ml))) (tail ml)

extractString :: ML -> [String]
extractString (ML ml) = extractString' [] ml

-- bool
extractBool'' :: MLItem -> [Bool]
extractBool'' (WrapBool bool) = [bool]
extractBool'' _ = []

extractBool' :: [Bool] -> [MLItem] -> [Bool]
extractBool' bl ml = if length ml == 0 then bl
    else extractBool' (bl ++ (extractBool'' (head ml))) (tail ml)

extractBool :: ML -> [Bool]
extractBool (ML ml) = extractBool' [] ml

-- count components
countComponents :: ML -> String
countComponents (ML ml) = "Strings: "
    ++ show (length (extractString' [] ml)) ++ ", Integers: "
    ++ show (length (extractInt' [] ml)) ++ ", Bools: "
    ++ show (length (extractBool' [] ml))
