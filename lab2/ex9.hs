qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart xs = filter (<x) xs
   rightPart xs = filter (>=x) xs

--mSort [] = []
--mSort xs = if length xs == 1
--    then xs
--    else mSort (leftSide xs) ++ mSort (rightPart xs)
--     where leftPart = take (length xs)/2 xs
--           rightPart = 

