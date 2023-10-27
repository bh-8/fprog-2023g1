isPrefixOf :: String -> String -> Bool
isPrefixOf s t = if (null s) then True
    else (head s == head t) && isPrefixOf (tail s) (tail t)
