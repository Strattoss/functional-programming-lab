fib :: (Num a, Eq a) => a -> a
fib n = 
    if n == 0 || n == 1 then n
    else fib(n-2) + fib(n-1)

sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || (or' xs)

and' :: [Bool] -> Bool
and' [] = False
and' (x:xs) = x && (and' xs)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) | a == x = True
    | otherwise = elem' a xs

doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll s = [x*2 | x <- s]

squareAll :: Num t => [t] -> [t]
squreAll [] = []
squareAll s = [x^2 | x <- s]

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven s = [x | x <- s, mod x 2 == 0]

-- rekursja z akumulatorem
sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
    where loop acc [] = acc
          loop acc (x:xs) = loop (x + acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
    where loop acc []     = acc
          loop acc (x:xs) = loop (x + acc) xs

prod'2 :: Num a => [a] -> a
prod'2 xs = loop 1 xs
    where loop acc [] = acc
          loop acc (x:xs) = loop (acc * x) xs

length'2 :: [a] -> Int
length'2 xs = loop 0 xs
    where loop acc [] = acc
          loop acc (x:xs) = loop (acc + 1) xs