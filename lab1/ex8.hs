not' :: Bool -> Bool
not' b = case b of
    True -> False
    False -> True

absInt n =
    case (n >= 0) of
        True -> n
        _ -> -n

isItTheAnswer :: String -> Bool
isItTheAnswer s = case s of
    "Love" -> True
    _ -> False

or' :: (Bool, Bool) -> Bool
or' (a, b) = case a == b && a == False of
    True -> False
    False -> True
