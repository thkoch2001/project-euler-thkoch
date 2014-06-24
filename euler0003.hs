
isDivisor :: Integer -> Integer -> Bool
isDivisor n d = (n `mod` d) == 0

findPrimeLarger :: Integer -> [Integer] -> Integer
findPrimeLarger n primes =
  if isPrime
    then n
    else findPrimeLarger (n + 2) primes
  where
    testPrimes = dropWhile (\x -> 2 * x > n) primes
    isPrime = not $ any (isDivisor n) testPrimes

nextPrime :: [Integer] -> [Integer]
nextPrime [2] = [3, 2]
nextPrime primes@(x:xs) = (findPrimeLarger (x + 2) primes) : primes
nextPrime _ = undefined

reduceWithPrimes :: Integer -> [Integer] -> Integer
reduceWithPrimes n primes@(x:xs)
  | n < x         = undefined
  | n == x        = n
  | isDivisor n x = reduceWithPrimes (n `div` x) primes
  | otherwise     = reduceWithPrimes n (nextPrime primes)

euler0003 :: Integer -> Integer
euler0003 n = reduceWithPrimes n [2]

