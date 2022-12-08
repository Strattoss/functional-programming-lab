onlyEven = filter' (\x -> x `mod` 2 == 0)

onlyOdd = filter' (\x -> x `mod` 2 == 1)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs
