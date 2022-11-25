isPalindrome :: [Char] -> Bool
isPalindrome xs = if (xs == [] ||
    length xs == 1 ||
    (head xs == last xs && isPalindrome (tail (init xs))))
    then True
    else False