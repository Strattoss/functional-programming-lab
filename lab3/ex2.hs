sum' :: Num a => [a] -> a
sum' = sumWith (1*)

sumSqr' :: Num a => [a] -> a
sumSqr' = sumWith (\x -> x^2)

sumCube' = sumWith (^3)

sumAbs' = sumWith (\x -> if x>=0 then x else -x)

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith _ [] = 0
sumWith f (x:xs) = f x + sumWith f xs