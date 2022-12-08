sum' :: Num a => [a] -> a
sum' = sumWith (1*)

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith _ [] = 0
sumWith f (x:xs) = f x + sumWith f xs

-- naprawdę nie wiem o co tu chodzi kompilatorowi
factorial :: (Num a, Eq a) => a->a
factorial 0 = 1
factorial n = n * factorial (n-1)

funcFactory n = \x -> x^n

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n = (\x -> sum' [x^k/factorial k | k <- [0..n]])
