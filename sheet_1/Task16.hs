asPrefixOf :: String -> String -> String
asPrefixOf p s
    | null s                = p
    | null p                = s
    | head (p) == head (s)  = head (s) : asPrefixOf (tail p) (tail s)
    | otherwise             = head (p) : asPrefixOf (tail p) s
