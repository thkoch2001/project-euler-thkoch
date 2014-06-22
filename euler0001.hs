isDivisible :: Integer -> Integer -> Bool
isDivisible divisor n = (n `mod` divisor) == 0

sumList :: [Integer] -> Integer
sumList = foldl (+) 0

euler0001 :: Integer -> Integer
euler0001 limit = sumList ([n | n <- [1..limit], (isDivisible 3 n) || (isDivisible 5 n) ])

