
sumEvenFibs :: Integer -> Integer -> Integer -> Integer -> Integer
sumEvenFibs limit sum n m = if n > limit
                              then sum
                              else sumEvenFibs limit newSum m (n + m)
                            where
                              newSum = if even n
                                         then sum + n
                                         else sum

euler0002 :: Integer -> Integer
euler0002 limit = sumEvenFibs limit 0 1 2