toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)


toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleFromLeft . reverse
    where doubleFromLeft [] = []
          doubleFromLeft (x:[]) = [x]
          doubleFromLeft (x:y:xs) = x : y * 2 : doubleFromLeft xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = (sum . toDigits) x
sumDigits (x:xs) = (sum . toDigits) x + sumDigits xs
  where sum :: [Integer] -> Integer
        sum [] = 0
        sum (x:[]) = x
        sum (x:xs) = x + sum xs

validate :: Integer -> Bool
validate n = (mod . reduce) n 10 == 0
  where reduce = sumDigits . doubleEveryOther  . toDigits
