fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

firstDividesSecond (x : y : _) = if mod y x == 0
    then True
    else False