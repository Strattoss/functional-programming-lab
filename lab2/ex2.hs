fiveToPower_ :: Integer -> Integer
fiveToPower_ a = 5^a

_ToPower5 :: Num a => a -> a
_ToPower5 a = a^5

substrNFrom5 :: Num a => a -> a
substrNFrom5 a = 5 - a

substr5From_ :: Num a => a -> a
substr5From_ a = a - 5