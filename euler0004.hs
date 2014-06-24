isPalindrome :: [Char] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs)
  | (x == l)  = isPalindrome $ init xs
  | otherwise = False
  where
    l = last xs

isPalindromic :: Integer -> Bool
isPalindromic = isPalindrome . show

palindromicNumbers :: Integer -> Integer -> [(Integer, Integer, Integer)]
palindromicNumbers u l = [(c, a, b) | a<-[u, u-1..l], b<-[a, a-1..l], c<-[a*b], isPalindromic c]

euler0004 :: (Integer, Integer, Integer)
euler0004 = maximum $ palindromicNumbers 999 100
