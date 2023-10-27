import Data.Maybe -- Maybe allows the value to be undefined

-- safeDiv (Just 8) (Just 2)
-- safeDiv (Just 8) (Just 0)
safeDiv :: Maybe Double -> Maybe Double -> Maybe Double
safeDiv x y
    | x == Nothing      = Nothing
    | y == Nothing      = Nothing
    | fromJust y == 0   = Nothing
    | otherwise         = Just ((fromJust x) / (fromJust y))

-- safeRoot (Just 9)
-- safeRoot (Just (-9))
safeRoot :: Maybe Double -> Maybe Double
safeRoot n
    | n == Nothing      = Nothing
    | fromJust n >= 0   = Just (sqrt (fromJust n))
    | otherwise         = Nothing
