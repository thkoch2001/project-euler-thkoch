
isDivisor :: Integer -> Integer -> Bool
isDivisor n d = (n `mod` d) == 0

isPrime :: Integer -> Bool
isPrime n = n `elem` [2, 3, 5, 7, 11, 13]
            || (not $ any (isDivisor n) $ 2:[3, 5..(n `div` 2)])

primeFactors :: Integer -> [Integer]
primeFactors n = [x | x <- 2:[3,5..(n `div` 2)], (isPrime x) && (isDivisor n x)]

euler0003 :: Integer -> Integer
euler0003 n = maximum $ primeFactors n