import Data.Char

doubleElems = map' (2*)
doubleElems' xs = [2*x | x <- xs]

sqrElems = map' (^2)
sqrElems' xs = [x^2 | x <- xs]

lowerCase = map' toLower
lowerCase' xs = [toLower x | x <- xs]

map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x fs = [f x | f <- fs]